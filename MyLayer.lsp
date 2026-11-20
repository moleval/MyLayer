;;; ============================================================
;;; MyLayer.lsp — МОНОЛИТ модулей слоёв AutoCAD
;;; Маркер редакции для RELOAD: (ред. 2)
;;;
;;; Объединение модулей:
;;;   секция 1: 01_Variables.lsp   (слои шаблона и системные переменные, LV:*)
;;;   секция 2: 02_LayerExport.lsp (выгрузка слоёв и фильтров в XML, LX:*, XML:*)
;;;   секция 3: 03_LayerImport.lsp (загрузка слоёв и фильтров из XML, LI:*)
;;;
;;; Загрузка через RELOAD (AutoExtraction, раздел PLUGINS)
;;; или напрямую: (load "MyLayer.lsp")
;;;
;;; Команды:
;;;   МОИСЛОИСОЗДАТЬ         - создать слои шаблона + применить переменные
;;;   МОИСЛОИПОУМОЛЧАНИЮ     - применить переменные слоёв
;;;   МОИСЛОИТЕКУЩИЕ         - показать текущие и требуемые значения переменных
;;;   МОИСЛОИВЫГРУЗКА        - выгрузить слои и фильтры в Слои и фильтры.xml
;;;   МОИСЛОИЗАГРУЗИТЬ       - загрузить слои, фильтры и применить переменные
;;;   МОИСЛОИФИЛЬТРЫ         - загрузить только фильтры (без слоёв)
;;;   МОИСЛОИФИЛЬТРЫОЧИСТИТЬ - удалить фильтры слоёв из XML
;;;   МОИФАСАДОБЛ            - вкл: Облицовка (Подсистема, Утепление, Железо выкл)
;;;   МОИФАСАДПОД            - вкл: Подсистема (Облицовка, Утепление, Железо выкл)
;;;   МОИФАСАДУТЕП           - вкл: Утепление (Подсистема, Облицовка, Железо выкл)
;;;   МОИФАСАДФАС            - вкл: Подсистема без слоёв подсистем + Железо
;;;   МОИСЛОИФИЛЬТРАР        - создать фильтр АР (всё, что не в фильтрах)
;;;   МОИСЛОИПРОВЕРКАФАЙЛА   - проверить баланс скобок в LISP-файле
;;;
;;; Шаблон фильтров (Слои и фильтры 2.xml):
;;;   Стройплэкс -> Витражи, Окна, Отключенные, Фасад
;;;   Фасад      -> Облицовка, Подсистема, Утепление
;;; Фильтр АР создаётся автоматически: слои чертежа без XREF и без
;;; слоёв, перечисленных в фильтрах шаблона.
;;; ============================================================

(vl-load-com)

;;; ============================================================
;;; СЕКЦИЯ 1: 01_Variables.lsp — слои шаблона и переменные слоёв
;;; ============================================================

;;; ========================= НАСТРОЙКИ =========================

;; Показывать пропущенные неподдерживаемые переменные.
(setq *LV:SHOW-SKIPPED* T)

;; Показывать таблицу созданных слоёв.
(setq *LV:SHOW-CREATED-LAYERS* T)

;;; ---------- Реестр слоёв, используемых переменными ----------
;;; Формат: (имя ACI тип_линии вес_код описание)
;;;
;;; Вес задаётся кодом DXF:
;;;   -3 "По умолчанию"
;;;   -2 "ByBlock"
;;;   -1 "ByLayer"
;;;    0 ... 211 = вес в сотых мм (9 = 0.09 мм, 13 = 0.13 мм...)

(setq *LV:LAYERS*
  '(
    ("0"                          7  "Continuous"    -3  "")
    ("Текст"                      2  "Continuous"    15  "Текстовый слой")
    ("Размеры"                   14  "Continuous"    13  "Размеры и таблицы")
    ("Штриховка"                 44  "Continuous"     9  "Слой штриховки")
    ("Обозначения"               14  "Continuous"     9  "Обозначения, выноски")
    ("Штрихпунктирные и осевые"  14  "GOST2.303 6"    0  "Осевые и центровые")
    ("Тонкая"                   110  "Continuous"    13  "Ревизионные облака")
    ("Невидимые"                250  "Continuous"    13  "Видовые экраны")
   )
)

;;; ---------- Системные переменные ----------
;;; Формат: (имя подсказка значение)

(setq *LV:VARS*
  '(
    ("TEXTLAYER"     "Текстовый слой"                 "Текст")
    ("DIMLAYER"      "Размерный слой"                 "Размеры")
    ("HPLAYER"       "Слой штриховки"                 "Штриховка")
    ("MLEADERLAYER"  "Слой мультивыносок"             "Обозначения")
    ("CENTERLAYER"   "Слой осевых и центровых линий"  "Штрихпунктирные и осевые")
    ("TABLELAYER"    "Слой таблиц"                    "Размеры")
    ("REVCLOUDLAYER" "Слой ревизионных облаков"       "Тонкая")
    ("VIEWPORTLAYER" "Слой видовых экранов"           "Невидимые")
    ("XREFLAYER"     "Слой внешних ссылок"            "0")
   )
)

;;; ============================================================
;;; Базовые функции
;;; ============================================================

(defun LV:IsString (x) (eq (type x) 'STR))

(defun LV:IsError (x)
  (if x (vl-catch-all-error-p x) nil)
)

(defun LV:CellStr (x)
  (cond
    ((null x) "")
    ((LV:IsString x) x)
    (t (vl-prin1-to-string x))
  )
)

(defun LV:Repeat (s n / out)
  (if (not (numberp n)) (setq n 0))
  (if (< n 0) (setq n 0))
  (setq out "")
  (repeat (fix n) (setq out (strcat out s)))
  out
)

(defun LV:Pad (s w / l)
  (setq s (LV:CellStr s))
  (if (not (numberp w)) (setq w 0))
  (setq l (strlen s))
  (if (>= l w)
    (strcat s " ")
    (strcat s (LV:Repeat " " (- w l)) " ")
  )
)

(defun LV:ListSetMax (lst idx val / i out len)
  (setq i 0) (setq out nil) (setq len (length lst))
  (cond
    ((< idx len)
      (foreach x lst
        (if (= i idx)
          (setq out (append out (list (max x val))))
          (setq out (append out (list x)))
        )
        (setq i (1+ i))
      )
    )
    (t
      (setq out lst)
      (while (< (length out) idx)
        (setq out (append out (list 0)))
      )
      (setq out (append out (list val)))
    )
  )
  out
)

(defun LV:MemberNoCase (name lst / found)
  (if (and (LV:IsString name) lst)
    (foreach x lst
      (if (and (LV:IsString x) (= (strcase x) (strcase name)))
        (setq found T)
      )
    )
  )
  found
)

(defun LV:AddUnique (lst name)
  (if (not (LV:IsString name))
    (setq name (vl-prin1-to-string name))
  )
  (if (or (not name) (= name ""))
    lst
    (if (LV:MemberNoCase name lst)
      lst
      (append lst (list name))
    )
  )
)

;;; ============================================================
;;; Табличный вывод
;;; ============================================================

(defun LV:PrintTable (title headers rows / widths i line sep)
  (if rows
    (progn
      (setq widths nil)
      (foreach h headers
        (setq widths (append widths (list (strlen (LV:CellStr h)))))
      )
      (foreach row rows
        (setq i 0)
        (foreach cell row
          (setq widths (LV:ListSetMax widths i (strlen (LV:CellStr cell))))
          (setq i (1+ i))
        )
      )

      (if (and title (/= title ""))
        (princ (strcat "\n" title))
      )

      (setq line "") (setq i 0)
      (foreach h headers
        (setq line (strcat line (LV:Pad h (nth i widths))))
        (setq i (1+ i))
      )
      (princ (strcat "\n" line))

      (setq sep "")
      (foreach w widths
        (setq sep (strcat sep (LV:Pad (LV:Repeat "-" w) w)))
      )
      (princ (strcat "\n" sep))

      (foreach row rows
        (setq line "") (setq i 0)
        (foreach cell row
          (setq line (strcat line (LV:Pad cell (nth i widths))))
          (setq i (1+ i))
        )
        (princ (strcat "\n" line))
      )
    )
  )
)

;;; ============================================================
;;; Работа с переменными
;;; ============================================================

(defun LV:GetVarRaw (var)
  (vl-catch-all-apply 'getvar (list var))
)

(defun LV:VarSupported (var / raw)
  (setq raw (LV:GetVarRaw var))
  (not (or (LV:IsError raw) (null raw)))
)

(defun LV:GetVarSafe (var / raw)
  (setq raw (LV:GetVarRaw var))
  (cond
    ((LV:IsError raw) "<ошибка>")
    ((null raw)       "<нет / не поддерживается>")
    ((LV:IsString raw) raw)
    (t (vl-prin1-to-string raw))
  )
)

(defun LV:SetVar (var val / res)
  (setq res (vl-catch-all-apply 'setvar (list var val)))
  (not (LV:IsError res))
)

;;; ============================================================
;;; Работа со слоями
;;; ============================================================

(defun LV:LayerExists (name)
  (if (not (LV:IsString name))
    (setq name (vl-prin1-to-string name))
  )
  (if (or (not name) (= name ""))
    nil
    (or (= (strcase name) "0") (tblsearch "LAYER" name))
  )
)

(defun LV:SetDxf (ent code val / old)
  (setq old (assoc code ent))
  (if old
    (subst (cons code val) old ent)
    (append ent (list (cons code val)))
  )
)

;;; Попытаться загрузить тип линии (acad.lin / acadiso.lin).
(defun LV:LoadLinetype (doc name / ltypes res)
  (if (and name (/= name "") (not (tblsearch "LTYPE" name)))
    (progn
      (setq ltypes (vl-catch-all-apply 'vla-get-Linetypes (list doc)))
      (if (not (LV:IsError ltypes))
        (progn
          (setq res (vl-catch-all-apply 'vlax-invoke (list ltypes 'Load name)))
          (if (LV:IsError res)
            (setq res (vl-catch-all-apply 'vlax-invoke (list ltypes 'Load name "")))
          )
        )
      )
    )
  )
  (tblsearch "LTYPE" name)
)

;;; Создать/обновить слой с полными свойствами.
;;; Возвращает:
;;;   "created" / "exists" / "failed"
;;; И добавляет в *LV:WARN-LAYERS* предупреждения по типам линий.

(defun LV:CreateLayerFull (doc name aci lt lw desc /
                            layers res ent status)
  (setq status "exists")

  (if (not (tblsearch "LAYER" name))
    (progn
      (setq layers (vl-catch-all-apply 'vla-get-Layers (list doc)))
      (if (LV:IsError layers)
        (setq status "failed")
        (progn
          (setq res (vl-catch-all-apply 'vla-add (list layers name)))
          (if (LV:IsError res)
            (setq status "failed")
            (setq status "created")
          )
        )
      )
    )
  )

  (if (not (= status "failed"))
    (progn
      (setq ent (entget (tblobjname "LAYER" name)))
      (if ent
        (progn
          ;; Цвет ACI
          (if (and aci (numberp aci) (>= aci 1) (<= aci 255))
            (setq ent (LV:SetDxf ent 62 (abs aci)))
          )

          ;; Тип линии
          (if (and lt (LV:IsString lt) (/= lt ""))
            (progn
              (if (not (tblsearch "LTYPE" lt))
                (LV:LoadLinetype doc lt)
              )
              (if (tblsearch "LTYPE" lt)
                (setq ent (LV:SetDxf ent 6 lt))
                (progn
                  (setq ent (LV:SetDxf ent 6 "Continuous"))
                  (setq *LV:WARN-LAYERS*
                    (append *LV:WARN-LAYERS*
                            (list (list name lt))))
                )
              )
            )
          )

          ;; Вес линии
          (if (numberp lw)
            (setq ent (LV:SetDxf ent 370 lw))
          )

          ;; Описание
          (if (and desc (LV:IsString desc) (/= desc ""))
            (setq ent (LV:SetDxf ent 3 desc))
          )

          (vl-catch-all-apply 'entmod (list ent))
        )
      )
    )
  )

  status
)

;;; ============================================================
;;; Основная логика
;;; ============================================================

(defun LV:SetAll
       (createAllLayers applyVars
        / doc item var hint val supported supportedCount setCount
          skipCount varRows createdLayers failedLayers layerStatus
          ok createdRows failedRows layRow lyName lyAci lyLt lyLw lyDesc
          layStatus)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq *LV:WARN-LAYERS* nil)

  (setq supportedCount 0)
  (setq setCount 0)
  (setq skipCount 0)

  (setq varRows nil)
  (setq createdLayers nil)
  (setq failedLayers nil)

  ;; ---------- Шаг 1: создать слои шаблона ----------

  (if createAllLayers
    (progn
      (princ "\n\n=== Создание слоёв шаблона ===")
      (princ (strcat "\n  "
                     (LV:Pad "Имя слоя" 30) "| "
                     (LV:Pad "ACI" 5)       "| "
                     (LV:Pad "Тип линии" 16) "| "
                     (LV:Pad "Вес" 8)       "| "
                     "Статус"))
      (princ (strcat "\n  "
                     (LV:Repeat "-" 30) "+"
                     (LV:Repeat "-" 6)  "+"
                     (LV:Repeat "-" 17) "+"
                     (LV:Repeat "-" 9)  "+"
                     (LV:Repeat "-" 12)))

      (foreach layRow *LV:LAYERS*
        (setq lyName (nth 0 layRow))
        (setq lyAci  (nth 1 layRow))
        (setq lyLt   (nth 2 layRow))
        (setq lyLw   (nth 3 layRow))
        (setq lyDesc (nth 4 layRow))

        (setq layStatus (LV:CreateLayerFull doc lyName lyAci lyLt lyLw lyDesc))

        (cond
          ((= layStatus "created")
            (setq createdLayers (LV:AddUnique createdLayers lyName))
          )
          ((= layStatus "failed")
            (setq failedLayers (LV:AddUnique failedLayers lyName))
          )
        )

        (princ (strcat "\n  "
                       (LV:Pad lyName 30) "| "
                       (LV:Pad lyAci 5)   "| "
                       (LV:Pad lyLt 16)   "| "
                       (LV:Pad (LV:WeightText lyLw) 8) "| "
                       (cond
                         ((= layStatus "created") "создан")
                         ((= layStatus "exists")  "уже есть")
                         (t                       "ошибка")))
        )
      )
    )
  )

  ;; ---------- Шаг 2: применить переменные ----------

  (if applyVars
    (progn
      (princ "\n\n=== Установка переменных ===")

      (foreach item *LV:VARS*
        (setq var  (nth 0 item))
        (setq hint (nth 1 item))
        (setq val  (nth 2 item))

        (setq supported (LV:VarSupported var))

        (if supported
          (progn
            (setq supportedCount (1+ supportedCount))
            (setq ok (LV:SetVar var val))

            (if ok
              (progn
                (setq setCount (1+ setCount))
                (setq varRows
                  (append varRows (list (list "Установлено" var val hint))))
              )
              (setq varRows
                (append varRows (list (list "Ошибка" var val hint))))
            )
          )
          (progn
            (setq skipCount (1+ skipCount))
            (if *LV:SHOW-SKIPPED*
              (setq varRows
                (append varRows (list (list "Не в этой версии" var val hint))))
            )
          )
        )
      )
    )
  )

  ;; ---------- Печать таблиц ----------

  (if varRows
    (LV:PrintTable
      ""
      '("Статус" "Переменная" "Значение" "Подсказка")
      varRows
    )
  )

  (if (and *LV:SHOW-CREATED-LAYERS* createdLayers)
    (progn
      (setq createdRows nil)
      (foreach n createdLayers
        (setq createdRows (append createdRows (list (list n)))))
      (LV:PrintTable "Созданные слои" '("Имя слоя") createdRows)
    )
  )

  (if failedLayers
    (progn
      (setq failedRows nil)
      (foreach n failedLayers
        (setq failedRows (append failedRows (list (list n)))))
      (LV:PrintTable "Не удалось создать слои" '("Имя слоя") failedRows)
    )
  )

  ;; Предупреждения по типам линий слоёв шаблона
  (if *LV:WARN-LAYERS*
    (progn
      (princ "\n\nПредупреждения по типам линий слоёв шаблона:")
      (princ (strcat "\n  "
                     (LV:Pad "Слой" 30) "| "
                     "Тип линии"))
      (princ (strcat "\n  "
                     (LV:Repeat "-" 30) "+"
                     (LV:Repeat "-" 20)))
      (foreach w *LV:WARN-LAYERS*
        (princ (strcat "\n  "
                       (LV:Pad (nth 0 w) 30) "| "
                       (nth 1 w) " -> Continuous"))
      )
    )
  )

  ;; Итог
  (if applyVars
    (progn
      (princ
        (strcat "\nУстановлено переменных: "
                (itoa setCount) " из " (itoa supportedCount) " поддерживаемых"))
      (if (> skipCount 0)
        (if *LV:SHOW-SKIPPED*
          (princ (strcat "\nПропущено (нет в этой версии AutoCAD): " (itoa skipCount)))
          (princ (strcat "\nПропущено (нет в этой версии AutoCAD, скрыто настройкой *LV:SHOW-SKIPPED*): " (itoa skipCount)))
        )
      )
    )
  )
)

;;; Показать вес линии по коду DXF в виде текста.
(defun LV:WeightText (w)
  (cond
    ((null w) "")
    ((not (numberp w)) (LV:CellStr w))
    ((= w -3) "По умолч")
    ((= w -2) "ByBlock")
    ((= w -1) "ByLayer")
    (t (strcat (rtos (/ w 100.0) 2 2) " мм"))
  )
)

;;; ============================================================
;;; Команды
;;; ============================================================

;; Создать слои шаблона и применить переменные.
(defun C:МОИСЛОИСОЗДАТЬ ()
  (LV:SetAll T T)
  (princ)
)

;; Только применить переменные (слои уже должны быть в чертеже).
(defun C:МОИСЛОИПОУМОЛЧАНИЮ ()
  (LV:SetAll nil T)
  (princ)
)

;; Показать текущие значения переменных.
(defun C:МОИСЛОИТЕКУЩИЕ ( / rows item var hint need)
  (setq rows nil)
  (foreach item *LV:VARS*
    (setq var  (nth 0 item))
    (setq hint (nth 1 item))
    (setq need (nth 2 item))
    (setq rows
      (append rows
              (list (list var (LV:GetVarSafe var) need hint))))
  )
  (LV:PrintTable
    "Текущие значения переменных"
    '("Переменная" "Текущее" "Нужно" "Подсказка")
    rows
  )
  (princ)
)

;;; ============================================================
;;; СЕКЦИЯ 2: 02_LayerExport.lsp — экспорт слоёв и фильтров в XML
;;; ============================================================

;;; ============================================================
;;; Базовые функции
;;; ============================================================

(defun LX:IsString (x) (eq (type x) 'STR))
(defun LX:IsError (x)  (if x (vl-catch-all-error-p x) nil))

(defun LX:ToStr (x)
  (cond
    ((null x) "")
    ((eq (type x) 'STR) x)
    ((eq (type x) 'INT) (itoa x))
    ((eq (type x) 'REAL) (rtos x 2 8))
    ((eq (type x) 'VLA-OBJECT) "")
    (t (vl-prin1-to-string x))
  )
)

(defun LX:PadRight (s width / n)
  (setq s (LX:ToStr s))
  (setq n (strlen s))
  (while (< n width) (setq s (strcat s " ")) (setq n (1+ n)))
  s
)

(defun LX:RepeatChar (c n / s)
  (setq s "")
  (while (> n 0) (setq s (strcat s c)) (setq n (1- n)))
  s
)

(defun LX:BoolText (x)
  (cond
    ((null x) "")
    ((eq x T) "Да") ((eq x :vlax-true) "Да") ((eq x :vlax-false) "Нет")
    ((numberp x) (if (= x 0) "Нет" "Да"))
    (t "Да")
  )
)

(defun LX:LineweightText (w)
  (cond
    ((null w) "")
    ((not (numberp w)) (LX:ToStr w))
    ((= w -3) "По умолчанию")
    ((= w -2) "ByBlock")
    ((= w -1) "ByLayer")
    (t (strcat (rtos (/ w 100.0) 2 2) " мм"))
  )
)

;;; ============================================================
;;; XML-функции
;;; ============================================================

(defun XML:Replace (s find rep / pos start out lenf)
  (if (not (eq (type s) 'STR)) (setq s (LX:ToStr s)))
  (if (or (not find) (= find "")) s
    (progn
      (setq lenf (strlen find) start 0 out "")
      (while (setq pos (vl-string-search find s start))
        (setq out (strcat out (substr s (1+ start) (- pos start)) rep))
        (setq start (+ pos lenf))
      )
      (setq out (strcat out (substr s (1+ start))))
      out
    )
  )
)

(defun XML:Escape (s)
  (setq s (LX:ToStr s))
  (setq s (XML:Replace s (chr 9)  " "))
  (setq s (XML:Replace s (chr 10) " "))
  (setq s (XML:Replace s (chr 13) " "))
  (setq s (XML:Replace s "&"  "&amp;"))
  (setq s (XML:Replace s "<"  "&lt;"))
  (setq s (XML:Replace s ">"  "&gt;"))
  (setq s (XML:Replace s "\"" "&quot;"))
  (setq s (XML:Replace s "'"  "&apos;"))
  s
)

(defun XML:Row (cells / row s)
  (setq row "<Row>")
  (foreach cell cells
    (setq s (XML:Escape cell))
    (setq row (strcat row
                      "<Cell><Data ss:Type=\"String\">"
                      s
                      "</Data></Cell>"))
  )
  (strcat row "</Row>")
)

;;; ============================================================
;;; Объекты ActiveX и свойства
;;; ============================================================

(defun LX:AsVla (x / obj)
  (cond
    ((eq (type x) 'VLA-OBJECT) x)
    ((eq (type x) 'ENAME)
      (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list x)))
      (if (LX:IsError obj) nil obj))
    (t nil)
  )
)

(defun LX:GetPropValue (obj prop / res)
  (setq obj (LX:AsVla obj))
  (if obj
    (progn
      (setq res (vl-catch-all-apply 'vlax-get (list obj prop)))
      (if (LX:IsError res) nil res)
    )
    nil
  )
)

(defun LX:PropStr (obj prop) (LX:ToStr (LX:GetPropValue obj prop)))

(defun LX:GetFirstPropValue (obj props / val)
  (foreach p props
    (if (null val) (setq val (LX:GetPropValue obj p))))
  val
)

(defun LX:GetFirstPropStr (obj props / s)
  (foreach p props
    (if (or (not s) (= s "")) (setq s (LX:PropStr obj p))))
  s
)

(defun LX:HasCount (obj / cnt)
  (setq cnt (LX:GetPropValue obj 'Count))
  (and (numberp cnt) (>= cnt 0))
)

;;; ============================================================
;;; Путь и файл экспорта
;;; ============================================================

(defun LX:OutFile (/ p out d)
  (setq p (getvar "DWGPREFIX"))
  (if (and p (/= p ""))
    (if (vl-file-directory-p p)
      (if (/= (substr p (strlen p) 1) "\\")
        (setq p (strcat p "\\")))
      (setq p (strcat (vl-filename-directory p) "\\")))
    (setq p nil))
  (if (or (not p) (= p "") (= p "\\"))
    (progn
      (setq p (getenv "TEMP"))
      (if (not p) (setq p "C:\\Temp"))
      (if (/= (substr p (strlen p) 1) "\\")
        (setq p (strcat p "\\")))))
  ;; *LX:OUT-FILE*: полный путь — как есть, имя файла — в папке чертежа.
  (setq out (if (and *LX:OUT-FILE*
                     (= (type *LX:OUT-FILE*) 'STR)
                     (/= *LX:OUT-FILE* ""))
              *LX:OUT-FILE*
              "Слои и фильтры.xml"))
  (setq d (vl-filename-directory out))
  (if (and d (= (type d) 'STR) (/= d ""))
    out
    (strcat p out))
)

;;; ============================================================
;;; Прозрачность слоя
;;; ============================================================

(defun LX:GetTransparency (layer / val obj tmp)
  (setq val (LX:GetFirstPropValue layer '(Transparency LayerTransparency)))
  (if (and val (eq (type val) 'VLA-OBJECT))
    (progn
      (setq obj val)
      (setq val (LX:GetFirstPropValue obj
                                      '(Percent Opacity Value Transparency)))))
  (if (and val (eq (type val) 'VARIANT))
    (progn
      (setq tmp (vl-catch-all-apply 'vlax-variant-value (list val)))
      (if (not (LX:IsError tmp)) (setq val tmp))))
  val
)

;;; ============================================================
;;; Получение TrueColor через ActiveX
;;;
;;; Возвращает список (R G B) если TrueColor задан,
;;; иначе nil.
;;;
;;; ColorMethod = 194 (acByColor) — TrueColor.
;;; Другие значения (192 ByLayer, 193 ByBlock, 195 ACI,
;;; 196 Pen, 197 DgnIndex) — НЕ TrueColor.
;;; ============================================================

(defun LX:GetTrueColorRGB (layerObj / tcObj cmethod r g b)
  (setq tcObj (vl-catch-all-apply 'vla-get-truecolor (list layerObj)))
  (if (or (LX:IsError tcObj) (null tcObj))
    nil
    (progn
      (setq cmethod (vl-catch-all-apply 'vla-get-colormethod
                                        (list tcObj)))
      (if (or (LX:IsError cmethod) (not (numberp cmethod)))
        nil
        (if (= cmethod 194)
          (progn
            (setq r (vl-catch-all-apply 'vla-get-red   (list tcObj)))
            (setq g (vl-catch-all-apply 'vla-get-green (list tcObj)))
            (setq b (vl-catch-all-apply 'vla-get-blue  (list tcObj)))
            (if (and (numberp r) (numberp g) (numberp b)
                     (>= r 0) (<= r 255)
                     (>= g 0) (<= g 255)
                     (>= b 0) (<= b 255))
              (list r g b)
              nil))
          nil)
      )
    )
  )
)

;;; ============================================================
;;; Экспорт слоёв
;;; ============================================================

(defun LX:WriteLayersSheet
       (fh doc onlySet / layers layer name ent flags color
              desc linetype lw plotflag transparency
              plotStyle colorName red green blue colorMethod
              row count allRows sortedRows rec
              layerObj rgb rVal gVal bVal tcAny totalCnt skippedCnt)
  (write-line "<Worksheet ss:Name=\"Слои\"><Table>" fh)
  (write-line
    (strcat
      " <Column ss:Width=\"189\"/><Column ss:Width=\"49.5\"/>"
      "<Column ss:Width=\"45.75\"/><Column ss:Width=\"47.25\"/>"
      "<Column ss:Width=\"57\"/><Column ss:Width=\"21\"/>"
      "<Column ss:Width=\"15.75\"/><Column ss:Width=\"21\"/>"
      "<Column ss:Width=\"64.5\"/><Column ss:Width=\"71.25\"/>"
      "<Column ss:Width=\"78\"/><Column ss:Width=\"67.5\"/>"
      "<Column ss:Width=\"64.5\"/><Column ss:Width=\"45\"/>"
      "<Column ss:Width=\"56.25\"/><Column ss:Width=\"114\"/>"
      "<Column ss:Width=\"69\"/><Column ss:Width=\"56.25\"/>"
      "<Column ss:Width=\"81.75\"/><Column ss:Width=\"46.5\"/>")
    fh)
  (write-line
    (XML:Row
      '("Имя слоя" "Описание" "Цвет ACI" "Цвет имя" "Цвет метод"
        "R" "G" "B" "Тип линии" "Вес линии код" "Вес линии текст"
        "Прозрачность" "Стиль печати" "Включен" "Заморожен"
        "Заморожен в новых ВЭ" "Заблокирован" "Печатается"
        "Зависит от XREF" "Флаги 70"))
    fh)

  (setq allRows nil)
  (setq totalCnt 0)
  (setq skippedCnt 0)
  (setq layers (vla-get-Layers doc))

  ;; ---------- Сбор данных ----------
  (vlax-for layer layers
    (setq totalCnt (1+ totalCnt))
    (setq name (LX:PropStr layer 'Name))
    (setq ent  (tblsearch "LAYER" name))

    (if ent
      (progn
        (setq flags    (cdr (assoc 70 ent)))
        (setq color    (cdr (assoc 62 ent)))
        (setq desc     (cdr (assoc 3 ent)))
        (setq linetype (cdr (assoc 6 ent)))
        (setq lw       (cdr (assoc 370 ent)))
        (setq plotflag (cdr (assoc 290 ent))))
      (progn
        (setq flags 0 color nil desc nil
              linetype nil lw nil plotflag nil)))

    (if (null flags) (setq flags 0))
    (if (or (not desc) (= desc ""))
      (setq desc (LX:PropStr layer 'Description)))
    (if (or (not linetype) (= linetype ""))
      (setq linetype (LX:PropStr layer 'Linetype)))
    (if (null lw)
      (setq lw (LX:GetPropValue layer 'Lineweight)))

    ;; ---------- TrueColor через ActiveX ----------
    (setq red nil green nil blue nil colorMethod "ACI")
    (setq layerObj (LX:AsVla layer))
    (if layerObj
      (progn
        (setq rgb (LX:GetTrueColorRGB layerObj))
        (if rgb
          (progn
            (setq red   (car rgb))
            (setq green (cadr rgb))
            (setq blue  (caddr rgb))
            (setq colorMethod "TrueColor")))))

    ;; ---------- Fallback: DXF-код 420 (новые версии) ----------
    (if (and (not (= colorMethod "TrueColor"))
             ent
             (setq rVal (cdr (assoc 420 ent)))
             (numberp rVal)
             (>= rVal 0))
      (progn
        (setq red   (logand (lsh rVal -16) 255))
        (setq green (logand (lsh rVal -8)  255))
        (setq blue  (logand rVal 255))
        (setq colorMethod "TrueColor")))

    ;; Если не TrueColor — R/G/B пустые
    (if (/= colorMethod "TrueColor")
      (setq red "" green "" blue "")
    )

    (setq transparency (LX:GetTransparency layer))
    (setq plotStyle    (LX:GetFirstPropStr layer
                                            '(PlotStyleName PlotStyle)))

    (setq row
      (list
        name
        desc
        (if color (abs color) "")
        ""                      ; Цвет имя — не заполняется
        colorMethod
        red green blue
        linetype
        lw
        (LX:LineweightText lw)
        transparency
        plotStyle
        (if (and color (< color 0)) "Нет" "Да")
        (if (= 1 (logand flags 1)) "Да" "Нет")
        (if (= 2 (logand flags 2)) "Да" "Нет")
        (if (= 4 (logand flags 4)) "Да" "Нет")
        (if (null plotflag) "Да" (LX:BoolText plotflag))
        (if (= 16 (logand flags 16)) "Да" "Нет")
        flags))

    (if (or (null onlySet) (LI:NameInList onlySet name))
      (setq allRows (append allRows (list (cons name row))))
      (setq skippedCnt (1+ skippedCnt))
    )
  )

  ;; ---------- Сортировка по имени без учёта регистра ----------
  (setq sortedRows
    (vl-sort allRows
      '(lambda (a b)
         (< (strcase (car a)) (strcase (car b))))))

  ;; ---------- Печать в командную строку ----------
  (princ "\n\n=== Экспорт слоёв ===")
  (if onlySet
    (princ (strcat "\n[Инфо] Только слои фильтра '" *LX:ROOT-FILTER*
                   "': " (itoa (length sortedRows))
                   " из " (itoa totalCnt)
                   " (пропущено: " (itoa skippedCnt) ")"))
  )
  (princ (strcat "\n  "
                 (LX:PadRight "Имя слоя"  45) "| "
                 (LX:PadRight "Цвет"       6) "| "
                 (LX:PadRight "Тип линии" 16) "| "
                 (LX:PadRight "Вес"       13) "| "
                 (LX:PadRight "Вкл"        5) "| "
                 (LX:PadRight "Зам"        5) "| "
                 "Блок"))
  (princ (strcat "\n  "
                 (LX:RepeatChar "-" 45) "+"
                 (LX:RepeatChar "-"  7) "+"
                 (LX:RepeatChar "-" 17) "+"
                 (LX:RepeatChar "-" 14) "+"
                 (LX:RepeatChar "-"  6) "+"
                 (LX:RepeatChar "-"  6) "+"
                 (LX:RepeatChar "-"  5)))

  (setq count 0)
  (setq tcAny nil)

  (foreach rec sortedRows
    (setq name (car rec))
    (setq row  (cdr rec))

    ;; Есть ли TrueColor?
    (if (and (nth 5 row) (/= (nth 5 row) ""))
      (setq tcAny T))

    ;; Запись в XML
    (write-line (XML:Row row) fh)

    ;; Печать
    (princ (strcat "\n  "
                   (LX:PadRight name 45) "| "
                   (LX:PadRight (strcat (LX:ToStr (nth 2 row))
                                        (if (and (nth 5 row)
                                                 (/= (nth 5 row) ""))
                                          "*" ""))
                                6) "| "
                   (LX:PadRight (nth  8 row) 16) "| "
                   (LX:PadRight (nth 10 row) 13) "| "
                   (LX:PadRight (nth 13 row)  5) "| "
                   (LX:PadRight (nth 14 row)  5) "| "
                   (nth 16 row)))

    (setq count (1+ count))
  )

  (if tcAny
    (princ (strcat "\n  * — у слоя задан TrueColor, "
                   "в XML заполнены колонки R, G, B")))

  (write-line "</Table></Worksheet>" fh)
  (princ "\n")
  count
)

;;; ============================================================
;;; Словари (для поиска фильтров слоёв)
;;; ============================================================

(defun LX:FindDictionaryObject (name / doc dicts d res ename lst)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq dicts (vl-catch-all-apply 'vlax-get (list doc 'Dictionaries)))
  (if (and (not (LX:IsError dicts)) (LX:HasCount dicts))
    (vlax-for d dicts
      (if (and (not res)
               (= (strcase (LX:PropStr d 'Name)) (strcase name)))
        (setq res d))))
  (if (not res)
    (progn
      (setq ename (vl-catch-all-apply 'dictsearch
                                      (list (namedobjdict) name)))
      (if (not (LX:IsError ename))
        (progn
          (if (eq (type ename) 'ENAME) (setq res (LX:AsVla ename)))
          (if (eq (type ename) 'LIST)
            (progn
              (setq lst (assoc -1 ename))
              (if lst (setq res (LX:AsVla (cdr lst))))))))))
  res
)

(defun LX:DictEntries (dict / ename result item name obj)
  (setq result nil ename nil)
  (cond
    ((eq (type dict) 'VLA-OBJECT)
      (setq ename (vl-catch-all-apply 'vlax-vla-object->ename (list dict))))
    ((eq (type dict) 'ENAME) (setq ename dict)))
  (if (LX:IsError ename) (setq ename nil))
  (if ename
    (progn
      (vl-catch-all-apply 'dictsearch (list ename))
      (setq item (vl-catch-all-apply 'dictnext (list ename)))
      (while (and item (not (LX:IsError item))
                  (or (eq (type item) 'LIST)
                      (eq (type item) 'ENAME)
                      (eq (type item) 'VLA-OBJECT)))
        (cond
          ((eq (type item) 'LIST)
            (setq name (cdr (assoc 3 item)))
            (setq obj  (cdr (assoc 350 item)))
            (if (not obj) (setq obj (cdr (assoc 360 item))))
            (if (and name obj) (setq result (cons (cons name obj) result))))
          ((eq (type item) 'ENAME)
            (setq name (LX:PropStr item 'Name))
            (if (= name "") (setq name "ITEM"))
            (setq result (cons (cons name item) result)))
          ((eq (type item) 'VLA-OBJECT)
            (setq name (LX:PropStr item 'Name))
            (if (= name "") (setq name "ITEM"))
            (setq result (cons (cons name item) result))))
        (setq item (vl-catch-all-apply 'dictnext (list ename))))))
  (reverse result)
)

(defun LX:DictionaryItems (dictName / dictObj result count i item name)
  (setq dictObj (LX:FindDictionaryObject dictName))
  (if dictObj
    (progn
      (setq count (LX:GetPropValue dictObj 'Count))
      (if (and (numberp count) (> count 0))
        (progn
          (setq count (fix count) i 0)
          (repeat count
            (setq item (vl-catch-all-apply 'vlax-invoke
                                           (list dictObj 'Item i)))
            (if (not (LX:IsError item))
              (progn
                (setq item (LX:AsVla item))
                (setq name (LX:PropStr item 'Name))
                (if (= name "") (setq name (itoa i)))
                (setq result (cons (cons name item) result))))
            (setq i (1+ i)))))
      (if (not result) (setq result (LX:DictEntries dictObj)))))
  (reverse result)
)

(defun LX:GetLayerFilters (/ names items)
  (setq names '("ACAD_LAYERFILTERS" "ACAD_LAYERFILTER"
                "AcadLayerFilters"   "ACAD_FILTER"))
  (foreach n names
    (if (not items) (setq items (LX:DictionaryItems n))))
  items
)

;;; ============================================================
;;; Экспорт фильтров
;;; ============================================================

;; Слои фильтра-корня «моих» слоёв (с вложенными).
;; nil — фильтр не найден или дерево не прочитано.
(defun LX:RootFilterLayers (/ tree rec found)
  (setq tree (LI:FilterTreeFull (LI:FilterRootDictData) nil))
  (foreach rec tree
    (if (and (not found)
             (= (LI:SafeStrCase (nth 0 rec))
                (LI:SafeStrCase *LX:ROOT-FILTER*)))
      (setq found (nth 3 rec))
    )
  )
  found
)

;; Поддерево фильтров: сам фильтр и все потомки (родитель раньше детей).
(defun LX:FilterSubtree (tree name / rec out child sub)
  (setq rec (LI:FindParsedDef tree name))
  (if rec
    (progn
      (setq out (list rec))
      (foreach child (nth 2 rec)
        (foreach sub (LX:FilterSubtree tree child)
          (setq out (append out (list sub)))
        )
      )
      out
    )
    nil
  )
)

;; Собственные слои фильтра: прямые минус слои всех потомков.
(defun LX:FilterOwnLayers (tree rec / childAll out child crec lyr)
  (setq childAll nil)
  (foreach child (nth 2 rec)
    (setq crec (LI:FindParsedDef tree child))
    (if crec
      (foreach lyr (nth 3 crec)
        (setq childAll (LI:SafeAddUnique childAll lyr))
      )
    )
  )
  (setq out nil)
  (foreach lyr (nth 1 rec)
    (if (not (LI:NameInList childAll lyr))
      (setq out (append out (list lyr)))
    )
  )
  out
)

;; Строка состава фильтра для листа «Фильтры»:
;; собственные слои, затем токены >>Дочерний.
(defun LX:FilterRowString (direct kids / s k)
  (setq s (LI:SafeListToComma direct))
  (foreach k kids
    (setq s (if (= s "")
              (strcat ">>" k)
              (strcat s ",>>" k))
    )
  )
  s
)

(defun LX:WriteFiltersSheet (fh / tree defs rec item name parent ownCnt
                                allCnt kids own rowStr count rootRec)
  (write-line "<Worksheet ss:Name=\"Фильтры\"><Table>" fh)
  (write-line
    " <Column ss:Width=\"189\"/><Column ss:Width=\"600\"/>"
    fh)
  (write-line (XML:Row '("Имя фильтра" "Список слоев")) fh)
  (princ "\n\n")
  (princ "\n=== Экспорт фильтров ===")
  (setq tree (LI:FilterTreeFull (LI:FilterRootDictData) nil))
  (if tree
    (progn
      (if (and *LX:ROOT-FILTER* (/= *LX:ROOT-FILTER* ""))
        (setq rootRec (LI:FindParsedDef tree *LX:ROOT-FILTER*))
      )
      (if rootRec
        (setq defs (LX:FilterSubtree tree *LX:ROOT-FILTER*))
        (progn
          (princ (strcat "\n[Предупр.] Фильтр-корень '"
                         *LX:ROOT-FILTER*
                         "' не найден — выгружены все пользовательские фильтры."))
          (foreach rec tree
            (setq name (nth 0 rec))
            (if (and (not (LI:IsSystemFilter name))
                     (not (LI:NameInList (list *LI:AR-FILTER-NAME*) name))
                     (not (LI:NameInList (list *LI:UNASSIGNED-FILTER-NAME*) name)))
              (setq defs (append defs (list rec)))
            )
          )
        )
      )
    )
    (progn
      (princ "\n[Предупр.] Не удалось прочитать дерево фильтров — выгружены только имена.")
      (foreach item (LX:GetLayerFilters)
        (setq name (car item))
        (if (and (not (LI:IsSystemFilter name))
                 (not (LI:NameInList (list *LI:AR-FILTER-NAME*) name))
                 (not (LI:NameInList (list *LI:UNASSIGNED-FILTER-NAME*) name)))
          (setq defs (append defs (list (list name nil nil nil))))
        )
      )
    )
  )
  (princ (strcat "\n  "
                 (LX:PadRight "Имя фильтра" 24) "| "
                 (LX:PadRight "Родитель" 14) "| "
                 (LX:PadRight "Своих" 6) "| "
                 (LX:PadRight "Всего" 6) "| Дети"))
  (princ (strcat "\n  "
                 (LX:RepeatChar "-" 24) "+"
                 (LX:RepeatChar "-" 15) "+"
                 (LX:RepeatChar "-" 7) "+"
                 (LX:RepeatChar "-" 7) "+"
                 (LX:RepeatChar "-" 20)))
  (setq count 0)
  (if defs
    (foreach rec defs
      (setq name   (nth 0 rec))
      (setq parent (LI:FindParentOf defs name))
      (setq kids   (nth 2 rec))
      (setq own    (LX:FilterOwnLayers tree rec))
      (setq ownCnt (length own))
      (setq allCnt (length (nth 3 rec)))
      (setq rowStr (LX:FilterRowString own kids))
      (write-line (XML:Row (list name rowStr)) fh)
      (princ (strcat "\n  "
                     (LX:PadRight name 24) "| "
                     (LX:PadRight (if parent parent "-") 14) "| "
                     (LX:PadRight (itoa ownCnt) 6) "| "
                     (LX:PadRight (itoa allCnt) 6) "| "
                     (LI:SafeListToComma kids)))
      (setq count (1+ count))
    )
    (princ "\n  (фильтры не найдены)")
  )
  (write-line "</Table></Worksheet>" fh)
  (princ "\n\n")
  count
)

;;; ============================================================
;;; Основная команда
;;; ============================================================

(defun LX:Export (/ doc fname fh tmpdir layerCount filterCount tpl myLayers)
  (vl-load-com)
  (princ "\n")
  (princ "\n=== Экспорт слоёв и фильтров в XML ===")
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq fname (LX:OutFile))
  (princ (strcat "\nЧертёж: " (getvar "DWGNAME")))
  (princ (strcat "\nФайл:   " fname))
  ;; Только «мои» слои — из фильтра-корня.
  (setq myLayers nil)
  (if *LX:ONLY-MY-LAYERS*
    (progn
      (setq myLayers (LX:RootFilterLayers))
      (if myLayers
        (princ (strcat "\n[Инфо] Слои фильтра-корня '" *LX:ROOT-FILTER*
                       "': " (itoa (length myLayers))))
        (princ (strcat "\n[Предупр.] Фильтр-корень '" *LX:ROOT-FILTER*
                       "' не найден — выгружаю все слои."))
      )
    )
  )
  ;; Предупреждение, если пишем поверх действующего шаблона.
  (setq tpl (LI:FindTemplateFile))
  (if (and tpl
           (findfile fname)
           (= (strcase (vl-filename-directory (findfile fname)))
              (strcase (vl-filename-directory tpl)))
           (= (strcase (vl-filename-base fname))
              (strcase (vl-filename-base tpl))))
    (princ (strcat "\n[Предупр.] Файл совпадает с шаблоном и будет перезаписан: "
                   tpl))
  )
  (if (findfile fname) (princ "\nФайл существует — перезаписываю."))
  (setq fh (open fname "w"))
  (if (not fh)
    (progn
      (setq tmpdir (getenv "TEMP"))
      (if (not tmpdir) (setq tmpdir "C:\\Temp"))
      (setq fname (strcat tmpdir "\\" (vl-filename-base fname) ".xml"))
      (setq fh (open fname "w"))
      (princ (strcat "\nОсновной путь недоступен. Пишу в: " fname))))
  (if fh
    (progn
      (write-line "<?xml version=\"1.0\" encoding=\"windows-1251\"?>" fh)
      (write-line "<?mso-application progid=\"Excel.Sheet\"?>" fh)
      (write-line
        (strcat
          "<Workbook "
          "xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\" "
          "xmlns:o=\"urn:schemas-microsoft-com:office:office\" "
          "xmlns:x=\"urn:schemas-microsoft-com:office:excel\" "
          "xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\" "
          "xmlns:html=\"http://www.w3.org/TR/REC-html40\">")
        fh)
      (write-line " <Styles>" fh)
      (write-line "  <Style ss:ID=\"Default\" ss:Name=\"Normal\">" fh)
      (write-line "   <Alignment ss:Vertical=\"Bottom\"/>" fh)
      (write-line "   <Borders/>" fh)
      (write-line "   <Font ss:FontName=\"Arial\"/>" fh)
      (write-line "   <Interior/>" fh)
      (write-line "   <NumberFormat/>" fh)
      (write-line "   <Protection/>" fh)
      (write-line "  </Style>" fh)
      (write-line " </Styles>" fh)
      (setq layerCount  (LX:WriteLayersSheet  fh doc myLayers))
      (setq filterCount (LX:WriteFiltersSheet fh))
      (write-line "</Workbook>" fh)
      (close fh)
      (princ "\n\nГотово.")
      (princ (strcat "\nФайл:               " fname))
      (princ (strcat "\nСлоёв выгружено:    " (itoa layerCount)))
      (princ (strcat "\nФильтров выгружено: " (itoa filterCount)))
      (princ "\n"))
    (princ "\nНе удалось создать файл экспорта."))
  (princ)
)

(defun C:МОИСЛОИВЫГРУЗКА () (LX:Export))

;;; ============================================================
;;; СЕКЦИЯ 3: 03_LayerImport.lsp — импорт слоёв и фильтров из XML
;;; ============================================================

(setq *LI:ALWAYS-ASK* nil)
(setq *LI:DELETE-EXISTING-FILTERS* nil)
(setq *LI:SET-CURRENT-FILTER* nil)
(setq *LI:WARN* nil)

;; ---------- Шаблоны, экспорт и фильтр АР ----------
(setq *LI:TEMPLATE-NAME*      "Слои и фильтры.xml")     ; основной шаблон для импорта
(setq *LI:TEMPLATE-FALLBACKS* '("Слои.xml"))
(setq *LX:OUT-FILE*           "Слои и фильтры.xml")     ; имя файла экспорта
(setq *LX:ROOT-FILTER*        "Стройплэкс")             ; корень «моих» фильтров и слоёв
(setq *LX:ONLY-MY-LAYERS*     T)          ; выгружать только слои из корня
(setq *LI:AUTO-AR*            T)          ; создавать АР при импорте
(setq *LI:AR-FILTER-NAME*     "АР")
(setq *LI:AR-SKIP-XREF*       T)          ; не включать XREF-слои (бит 16 кода 70)
(setq *LI:AR-SKIP-LAYERS*     '("0" "DEFPOINTS"))
(setq *LI:AR-CLEAN-WITH-FILTERS* T)       ; очистка удаляет АР и Нераспределённые
(setq *LI:AR-LAST-COUNT*      nil)        ; итог последнего построения АР
(setq *LI:AR-LAST-XREF*       0)          ; пропущено XREF в последнем построении
(setq *LI:ARF-NAME-OVERRIDE*  nil)        ; временное имя целевого фильтра

;; ---------- Миграция: фильтр «Нераспределённые» ----------
(setq *LI:AUTO-UNASSIGNED*        T)      ; создавать при импорте
(setq *LI:UNASSIGNED-FILTER-NAME* "Нераспределённые")
(setq *LI:LEGACY-FILTER-MASKS*  '("ВИТРАЖ" "ФОНАР" "ФАСАД")) ; подстроки, без регистра
(setq *LI:DELETE-ALL-FILTERS*     T)      ; удалять все пользовательские фильтры
(setq *LI:UNASSIGNED-LAST-COUNT*  nil)
(setq *LI:UNASSIGNED-LAST-XREF*   0)

;; Старые фильтры слоёв (удаляются при импорте и очистке фильтров)
(setq *LI:OLD-FILTERS*
  '(
    "Фасад - Облицовка"
    "Фасад - Подсистема"
    "Фасад - Утепление"
    "Фасады"
    "Фонарь 3D"
    "АР"
    "Нераспределённые"
  )
)

;; Системные фильтры AutoCAD: не удаляются и не экспортируются.
(setq *LI:SYSTEM-FILTERS*
  '("Все" "Все используемые слои" "Все слои без внешних ссылок"
    "Вн-ссылки" "Внешние ссылки" "XREF"))

;; Системный ли фильтр: имя из списка (без учёта регистра)
;; или любое имя, начинающееся с "*".
(defun LI:IsSystemFilter (name / s)
  (setq s (LI:SafeTrim name))
  (if (= s "")
    nil
    (or (= (substr s 1 1) "*")
        (LI:NameInList *LI:SYSTEM-FILTERS* s))
  )
)

;; Имена фильтров фасадных групп (новый и старый шаблон)
(setq *LI:FACADE-OBL-FILTERS*  '("Фасад - Облицовка" "Облицовка"))
(setq *LI:FACADE-POD-FILTERS*  '("Фасад - Подсистема" "Подсистема"))
(setq *LI:FACADE-UTEP-FILTERS* '("Фасад - Утепление" "Утепление" "Утеплитель"))

;; Резервные списки слоёв фасадных групп (если фильтры ещё не созданы в чертеже)
(setq *LI:FACADE-OBL-DEFAULT-LAYERS*
  '(
    "Вентиляционные решетки"
    "Водосток"
    "Кассеты"
    "Керамогранит"
    "Ламели"
    "Обозначения узлов разрезов и сечений"
    "Отметки"
    "Подшивка"
    "Размеры облицовки"
  )
)

(setq *LI:FACADE-POD-DEFAULT-LAYERS*
  '(
    "Контур КЖ"
    "Контур КМ"
    "Контур стен"
    "Отметки"
    "Подсистема"
    "Подсистема алюминиевая"
    "Подсистема оцинкованная"
    "Размеры подсистемы"
  )
)

(setq *LI:FACADE-UTEP-DEFAULT-LAYERS*
  '(
    "Контур КЖ"
    "Контур КМ"
    "Контур стен"
    "Контуры штриховки утеплителя"
    "Молниезащита"
    "Отметки"
    "Размеры утеплителя"
    "Штриховка утеплителя"
  )
)

;; Слои подсистем, которые выключаются в режиме МОИФАСАДФАС
(setq *LI:FACADE-SUBSYS-LAYERS*
  '(
    "Подсистема"
    "Подсистема алюминиевая"
    "Подсистема оцинкованная"
    "Размеры подсистемы"
  )
)

;; Слой фасонки / железа
(setq *LI:FACADE-IRON-LAYER* "Железо")

;;; ============================================================
;;; Базовые безопасные функции
;;; ============================================================

(defun LI:IsString (x) (eq (type x) 'STR))
(defun LI:IsError (x)  (if x (vl-catch-all-error-p x) nil))

(defun LI:ForceString (x)
  (cond
    ((null x) "")
    ((LI:IsString x) x)
    (t (vl-prin1-to-string x))
  )
)

(defun LI:IsSpaceChar (c)
  (or (= c " ") (= c (chr 9)) (= c (chr 10)) (= c (chr 13)))
)

(defun LI:SafeTrim (s / n)
  (setq s (LI:ForceString s))
  (while (and (> (strlen s) 0) (LI:IsSpaceChar (substr s 1 1)))
    (setq s (substr s 2))
  )
  (setq n (strlen s))
  (while (and (> n 0) (LI:IsSpaceChar (substr s n 1)))
    (setq s (substr s 1 (1- n)))
    (setq n (1- n))
  )
  s
)

(defun LI:Trim (s) (LI:SafeTrim s))

(defun LI:PadRight (s width / n)
  (setq s (LI:ForceString s))
  (setq n (strlen s))
  (while (< n width)
    (setq s (strcat s " "))
    (setq n (1+ n))
  )
  s
)

(defun LI:RepeatChar (c n / s)
  (setq s "")
  (while (> n 0)
    (setq s (strcat s c))
    (setq n (1- n))
  )
  s
)

(defun LI:AddWarn (name category description)
  (setq *LI:WARN*
    (append *LI:WARN*
            (list (list (LI:ForceString name)
                        (LI:ForceString category)
                        (LI:ForceString description)))))
)

(defun LI:PrintWarnings ( / )
  (if *LI:WARN*
    (progn
      (princ "\n\n")
      (princ "\nПредупреждения:")
      (princ (strcat "\n  "
                     (LI:PadRight "Слой" 45)
                     "| "
                     (LI:PadRight "Категория" 12)
                     "| Описание"))
      (princ (strcat "\n  "
                     (LI:RepeatChar "-" 45) "+"
                     (LI:RepeatChar "-" 13) "+"
                     (LI:RepeatChar "-" 30)))
      (foreach w *LI:WARN*
        (princ (strcat "\n  "
                       (LI:PadRight (nth 0 w) 45) "| "
                       (LI:PadRight (nth 1 w) 12) "| "
                       (nth 2 w)))
      )
      (princ "\n\n")
    )
  )
)

(defun LI:AsVla (x / obj)
  (cond
    ((= (type x) 'VLA-OBJECT) x)
    ((= (type x) 'ENAME)
      (setq obj (vl-catch-all-apply 'vlax-ename->vla-object (list x)))
      (if (LI:IsError obj) nil obj)
    )
    (t nil)
  )
)

(defun LI:Replace (s find rep / pos start out lenf)
  (setq s    (LI:ForceString s))
  (setq find (LI:ForceString find))
  (setq rep  (LI:ForceString rep))
  (if (or (= s "") (= find ""))
    s
    (progn
      (setq lenf (strlen find))
      (setq start 0)
      (setq out "")
      (while (setq pos (vl-string-search find s start))
        (setq out (strcat out (substr s (1+ start) (- pos start)) rep))
        (setq start (+ pos lenf))
      )
      (setq out (strcat out (substr s (1+ start))))
      out
    )
  )
)

(defun LI:XmlUnescape (s)
  (setq s (LI:Replace s "&lt;"   "<"))
  (setq s (LI:Replace s "&gt;"   ">"))
  (setq s (LI:Replace s "&quot;" "\""))
  (setq s (LI:Replace s "&apos;" "'"))
  (setq s (LI:Replace s "&amp;"  "&"))
  s
)

(defun LI:ToInt (s / v)
  (setq s (LI:SafeTrim s))
  (if (= s "")
    nil
    (progn
      (setq v (distof s))
      (if (numberp v) (fix v) nil)
    )
  )
)

(defun LI:YesNoTrue (s / tstr)
  (setq tstr (strcase (LI:SafeTrim s)))
  (or (= tstr "ДА") (= tstr "YES") (= tstr "TRUE") (= tstr "1")
      (= tstr "-1") (= tstr "ON") (= tstr "ИСТИНА"))
)

;;; ============================================================
;;; Чтение XML
;;; ============================================================

(defun LI:ReadFileWithCharset (fname charset / stream txt)
  (setq stream (vl-catch-all-apply 'vlax-create-object (list "ADODB.Stream")))
  (if (and stream (eq (type stream) 'VLA-OBJECT))
    (progn
      (vl-catch-all-apply 'vlax-put (list stream 'Type 2))
      (vl-catch-all-apply 'vlax-put (list stream 'Charset charset))
      (vl-catch-all-apply 'vlax-invoke (list stream 'Open))
      (vl-catch-all-apply 'vlax-invoke (list stream 'LoadFromFile fname))
      (setq txt (vl-catch-all-apply 'vlax-get (list stream 'ReadText)))
      (vl-catch-all-apply 'vlax-invoke (list stream 'Close))
      (vl-catch-all-apply 'vlax-release-object (list stream))
      (if (and txt (eq (type txt) 'STR) (> (strlen txt) 0))
        txt
        nil
      )
    )
    nil
  )
)

;;; Прочитать пролог и вернуть объявленную кодировку.
;;; vl-string-search возвращает 0-based позицию,
;;; substr работает с 1-based — учитываем это в индексах.
(defun LI:ReadXmlEncoding (fname / f line p1 p2 c enc)
  (setq enc nil)
  (if (setq f (open fname "r"))
    (progn
      (setq line (read-line f))
      (close f)
      (if (and line (eq (type line) 'STR) (> (strlen line) 0))
        (progn
          (setq p1 (vl-string-search "encoding=" line))
          (if p1
            (progn
              (setq p1 (+ p1 9))                 ; 0-based после "encoding="
              (setq c (substr line (1+ p1) 1))   ; 1-based чтение
              (if (or (= c "\"") (= c "'"))
                (setq p1 (1+ p1))                ; пропустить кавычку
              )
              (setq p2 (vl-string-search "\"" line p1))
              (if (not p2)
                (setq p2 (vl-string-search "'" line p1))
              )
              (if p2
                (setq enc (substr line (1+ p1) (- p2 p1)))
              )
            )
          )
        )
      )
    )
  )
  enc
)

(defun LI:MapCharset (decl / d)
  (setq d (strcase (LI:ForceString decl)))
  (cond
    ((or (= d "WINDOWS-1251") (= d "CP1251")) "windows-1251")
    ((or (= d "UTF-8") (= d "UTF8"))           "utf-8")
    ((or (= d "UTF-16") (= d "UTF-16LE") (= d "UNICODE")) "unicode")
    (t decl)
  )
)

(defun LI:ReadFile (fname / decl cs charsets s best first)
  (setq best nil)
  (setq first nil)
  (setq decl (LI:ReadXmlEncoding fname))
  (if decl
    (progn
      (setq cs (LI:MapCharset decl))
      (setq s (LI:ReadFileWithCharset fname cs))
      (if s
        (progn
          (if (or (vl-string-search "Слои" s)
                  (vl-string-search "Имя слоя" s)
                  (vl-string-search "Фильтры" s))
            (setq best s)
            (if (vl-string-search "<Worksheet" s)
              (setq first s)
            )
          )
        )
      )
    )
  )
  (if (not best)
    (progn
      (setq charsets (list "utf-8" "windows-1251" "unicode"))
      (foreach cs charsets
        (if (not best)
          (progn
            (setq s (LI:ReadFileWithCharset fname cs))
            (if s
              (progn
                (if (or (vl-string-search "Слои" s)
                        (vl-string-search "Имя слоя" s)
                        (vl-string-search "Фильтры" s))
                  (setq best s)
                  (if (and (not first)
                           (vl-string-search "<Worksheet" s))
                    (setq first s)
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  (if best best first)
)

(defun LI:GetWorksheet (xml name / pattern pos start end)
  (setq pattern (strcat "<Worksheet ss:Name=\"" name "\">"))
  (setq pos (vl-string-search pattern xml))
  (if pos
    (progn
      (setq start (+ pos (strlen pattern)))
      (setq end (vl-string-search "</Worksheet>" xml start))
      (if end (substr xml (1+ start) (- end start)) nil)
    )
    nil
  )
)

(defun LI:GetFirstWorksheet (xml / pos tagEnd end)
  (setq pos (vl-string-search "<Worksheet" xml))
  (if pos
    (progn
      (setq tagEnd (vl-string-search ">" xml pos))
      (if tagEnd
        (progn
          (setq end (vl-string-search "</Worksheet>" xml tagEnd))
          (if end (substr xml (+ tagEnd 2) (- end tagEnd 1)) nil)
        )
        nil
      )
    )
    nil
  )
)

(defun LI:GetRows (sheet / rows pos tagEnd end row)
  (setq rows nil)
  (setq pos 0)
  (while (setq pos (vl-string-search "<Row" sheet pos))
    (setq tagEnd (vl-string-search ">" sheet pos))
    (if tagEnd
      (progn
        (setq end (vl-string-search "</Row>" sheet tagEnd))
        (if end
          (progn
            (setq row (substr sheet (+ tagEnd 2) (- end tagEnd 1)))
            (setq rows (append rows (list row)))
            (setq pos (+ end 6))
          )
          (setq pos (strlen sheet))
        )
      )
      (setq pos (strlen sheet))
    )
  )
  rows
)

;;; ------------------------------------------------------------
;;; Разбор <Row> в список значений.
;;;
;;; Особенности Excel XML 2003:
;;;   - пустая ячейка = <Cell></Cell> (без <Data>) ИЛИ вовсе
;;;     отсутствует (тогда следующая имеет ss:Index="N");
;;;   - ss:Index задаёт 1-based номер колонки.
;;;
;;; ВАЖНО про индексы: vl-string-search возвращает 0-based
;;; позицию, substr работает с 1-based. Все внутренние
;;; переменные храним в 0-based; конверсия — только перед
;;; substr (старт = pos+1, длина = end - start).
;;; ------------------------------------------------------------

(defun LI:GetCellValues
       (row / vals pos cellStart tagEnd dataStart dataEnd cellEnd
        val ssPos eqPos ssEnd idxStr idx curIdx
        contentStart contentEnd tagEnd2)
  (setq vals nil)
  (setq pos 0)
  (setq curIdx 1)
  (while (setq cellStart (vl-string-search "<Cell" row pos))
    (setq tagEnd (vl-string-search ">" row cellStart))
    (if (not tagEnd)
      (setq pos (strlen row))
      (progn
        ;; ---------- ss:Index ----------
        (setq ssPos (vl-string-search "ss:Index=" row cellStart))
        (if (and ssPos (< ssPos tagEnd))
          (progn
            (setq eqPos (vl-string-search "=" row ssPos))
            (if eqPos
              (progn
                (setq ssPos (1+ eqPos))         ; 0-based после "="
                (if (or (= (substr row (1+ ssPos) 1) "\"")
                        (= (substr row (1+ ssPos) 1) "'"))
                  (setq ssPos (1+ ssPos))       ; пропустить кавычку
                )
                (setq ssEnd (vl-string-search "\"" row ssPos))
                (if (not ssEnd)
                  (setq ssEnd (vl-string-search "'" row ssPos))
                )
                (if ssEnd
                  (progn
                    (setq idxStr (substr row (1+ ssPos) (- ssEnd ssPos)))
                    (setq idx (atoi idxStr))
                    (if (> idx 0) (setq curIdx idx))
                  )
                )
              )
            )
          )
        )
        ;; ---------- добить пропущенные колонки ----------
        (while (< (length vals) (1- curIdx))
          (setq vals (append vals (list "")))
        )
        ;; ---------- закрытие </Cell> ----------
        (setq cellEnd (vl-string-search "</Cell>" row tagEnd))
        (if (not cellEnd)
          (setq cellEnd (strlen row))
        )
        ;; ---------- содержимое <Data>...</Data> ----------
        (setq val nil)
        (setq dataStart (vl-string-search "<Data" row tagEnd))
        (if (and dataStart (< dataStart cellEnd))
          (progn
            (setq tagEnd2 (vl-string-search ">" row dataStart))
            (if (and tagEnd2 (< tagEnd2 cellEnd))
              (progn
                (setq contentStart (1+ tagEnd2))     ; 0-based первого символа
                (setq contentEnd (vl-string-search "</Data>" row contentStart))
                (if (and contentEnd (< contentEnd cellEnd))
                  (setq val (substr row
                                    (1+ contentStart)
                                    (- contentEnd contentStart)))
                )
              )
            )
          )
        )
        (if (null val) (setq val ""))
        (setq vals (append vals (list (LI:XmlUnescape val))))
        (setq curIdx (1+ curIdx))
        (setq pos (+ cellEnd 7))
      )
    )
  )
  vals
)

;;; ============================================================
;;; Заголовки
;;; ============================================================

(defun LI:FixedHeaderMap ()
  (list
    (cons "ИМЯ СЛОЯ" 0) (cons "ОПИСАНИЕ" 1) (cons "ЦВЕТ ACI" 2)
    (cons "ЦВЕТ ИМЯ" 3) (cons "ЦВЕТ МЕТОД" 4) (cons "R" 5)
    (cons "G" 6) (cons "B" 7) (cons "ТИП ЛИНИИ" 8)
    (cons "ВЕС ЛИНИИ КОД" 9) (cons "ВЕС ЛИНИИ ТЕКСТ" 10)
    (cons "ПРОЗРАЧНОСТЬ" 11) (cons "СТИЛЬ ПЕЧАТИ" 12)
    (cons "ВКЛЮЧЕН" 13) (cons "ЗАМОРОЖЕН" 14)
    (cons "ЗАМОРОЖЕН В НОВЫХ ВЭ" 15) (cons "ЗАБЛОКИРОВАН" 16)
    (cons "ПЕЧАТАЕТСЯ" 17) (cons "ЗАВИСИТ ОТ XREF" 18) (cons "ФЛАГИ 70" 19)
  )
)

(defun LI:BuildHeaderMap (headers / i map h)
  (setq i 0)
  (setq map nil)
  (foreach h headers
    (setq map (append map (list (cons (strcase (LI:SafeTrim h)) i))))
    (setq i (1+ i))
  )
  (if (assoc "ИМЯ СЛОЯ" map) map (LI:FixedHeaderMap))
)

(defun LI:GetByHeader (vals map header / idx)
  (setq idx (cdr (assoc (strcase (LI:SafeTrim header)) map)))
  (if (and idx (< idx (length vals))) (nth idx vals) "")
)

;;; ============================================================
;;; Работа со слоями
;;; ============================================================

(defun LI:EnsureLayer (doc name / layers res)
  (if (tblsearch "LAYER" name)
    T
    (progn
      (setq layers (vla-get-Layers doc))
      (setq res (vl-catch-all-apply 'vla-add (list layers name)))
      (not (LI:IsError res))
    )
  )
)

(defun LI:SetDxf (ent code val / old)
  (setq old (assoc code ent))
  (if old
    (subst (cons code val) old ent)
    (append ent (list (cons code val)))
  )
)

(defun LI:SetBit (flags bit on)
  (if on
    (logior flags bit)
    (if (= (logand flags bit) bit)
      (- flags bit)
      flags
    )
  )
)

(defun LI:LoadLinetype (doc name / ltypes res)
  (if (and name (/= name "") (not (tblsearch "LTYPE" name)))
    (progn
      (setq ltypes (vl-catch-all-apply 'vla-get-Linetypes (list doc)))
      (if (not (LI:IsError ltypes))
        (progn
          (setq res (vl-catch-all-apply 'vlax-invoke (list ltypes 'Load name)))
          (if (LI:IsError res)
            (setq res (vl-catch-all-apply 'vlax-invoke
                                          (list ltypes 'Load name "")))
          )
        )
      )
    )
  )
  (tblsearch "LTYPE" name)
)

(defun LI:SetTransparency (layer val / num res obj)
  (setq num (LI:ToInt val))
  (if num
    (progn
      (setq res (vl-catch-all-apply 'vlax-put (list layer 'Transparency num)))
      (if (LI:IsError res)
        (progn
          (setq obj (vl-catch-all-apply 'vlax-get (list layer 'Transparency)))
          (if (not (LI:IsError obj))
            (progn
              (setq res (vl-catch-all-apply 'vlax-put (list obj 'Percent num)))
              (if (LI:IsError res)
                (vl-catch-all-apply 'vlax-put (list obj 'Value num))
              )
            )
          )
        )
      )
    )
  )
)

(defun LI:SetPlotStyle (layer val / res)
  (setq res (vl-catch-all-apply 'vlax-put (list layer 'PlotStyleName val)))
  (if (LI:IsError res)
    (vl-catch-all-apply 'vlax-put (list layer 'PlotStyle val))
  )
)

(defun LI:ApplyTrueColor (layerObj r g b / tcObj res1 res2)
  (if (and layerObj r g b
           (>= r 0) (<= r 255)
           (>= g 0) (<= g 255)
           (>= b 0) (<= b 255))
    (progn
      (setq tcObj (vl-catch-all-apply 'vla-get-truecolor (list layerObj)))
      (if (LI:IsError tcObj)
        nil
        (progn
          (setq res1 (vl-catch-all-apply 'vla-setrgb (list tcObj r g b)))
          (if (LI:IsError res1)
            nil
            (progn
              (setq res2 (vl-catch-all-apply 'vla-put-truecolor
                                             (list layerObj tcObj)))
              (not (LI:IsError res2))
            )
          )
        )
      )
    )
    T
  )
)

;;; ------------------------------------------------------------
;;; Применить одну строку слоя.
;;;
;;; Возвращает:
;;;   "Создан"    — новый слой
;;;   "Обновлён"  — существующий слой обновлён
;;;   "Ошибка"    — не удалось
;;;   "Пропущен"  — XREF-слой, не изменяем
;;; ------------------------------------------------------------

(defun LI:ApplyLayerRow (doc vals map / name isNew ename ent flags aci
                          onStr on oldColor colorVal desc lt lw plotStr res
                          layerObj transStr plotStyle ent2 c62 f70 tc2
                          rVal gVal bVal xrefSkip)
  (setq name (LI:SafeTrim (LI:GetByHeader vals map "Имя слоя")))
  (if (= name "")
    nil
    (progn
      (setq isNew (not (tblsearch "LAYER" name)))
      (if (not (LI:EnsureLayer doc name))
        (progn
          (LI:AddWarn name "ошибка" "не удалось создать слой")
          (princ (strcat "\n[Ошибка]    " (LI:PadRight name 45)
                         "| не удалось создать слой"))
          "Ошибка"
        )
        (progn
          (setq ename (tblobjname "LAYER" name))
          (if (not ename)
            (progn
              (LI:AddWarn name "ошибка" "не найден объект слоя")
              (princ (strcat "\n[Ошибка]    " (LI:PadRight name 45)
                             "| не найден объект слоя"))
              "Ошибка"
            )
            (progn
              (setq ent (entget ename))

              ;; ---------- Проверка XREF ----------
              (setq f70 (cdr (assoc 70 ent)))
              (setq xrefSkip nil)
              (if (and f70 (numberp f70) (= 16 (logand f70 16)))
                (progn
                  (LI:AddWarn name "XREF"
                              "слой из внешней ссылки, изменён не будет")
                  (princ (strcat "\n[XREF]      " (LI:PadRight name 45)
                                 "| пропущен (внешняя ссылка)"))
                  (setq xrefSkip T)
                )
              )

              (if xrefSkip
                "Пропущен"
                (progn
                  (setq desc (LI:SafeTrim (LI:GetByHeader vals map "Описание")))
                  (if (/= desc "") (setq ent (LI:SetDxf ent 3 desc)))

                  (setq lt (LI:SafeTrim (LI:GetByHeader vals map "Тип линии")))
                  (if (/= lt "")
                    (progn
                      (if (not (tblsearch "LTYPE" lt)) (LI:LoadLinetype doc lt))
                      (if (tblsearch "LTYPE" lt)
                        (setq ent (LI:SetDxf ent 6 lt))
                        (if (tblsearch "LTYPE" "Continuous")
                          (progn
                            (setq ent (LI:SetDxf ent 6 "Continuous"))
                            (LI:AddWarn name "тип линии"
                                        (strcat lt " -> Continuous"))
                          )
                          (LI:AddWarn name "тип линии"
                                      (strcat lt " (не найден)"))
                        )
                      )
                    )
                  )

                  (setq aci (LI:ToInt (LI:GetByHeader vals map "Цвет ACI")))
                  (if (and aci (>= aci 1) (<= aci 255))
                    (progn
                      (if isNew
                        (progn
                          (setq onStr (LI:SafeTrim
                                        (LI:GetByHeader vals map "Включен")))
                          (if (= onStr "")
                            (setq on T)
                            (setq on (LI:YesNoTrue onStr))
                          )
                          (setq colorVal (abs aci))
                          (if (not on) (setq colorVal (- colorVal)))
                          (setq ent (LI:SetDxf ent 62 colorVal))
                        )
                        (progn
                          (setq oldColor (cdr (assoc 62 ent)))
                          (if (and oldColor (/= oldColor 0))
                            (if (< oldColor 0)
                              (setq colorVal (- (abs aci)))
                              (setq colorVal (abs aci))
                            )
                            (setq colorVal (abs aci))
                          )
                          (setq ent (LI:SetDxf ent 62 colorVal))
                        )
                      )
                    )
                  )

                  (setq lw (LI:ToInt (LI:GetByHeader vals map "Вес линии код")))
                  (if (numberp lw) (setq ent (LI:SetDxf ent 370 lw)))

                  (if isNew
                    (progn
                      (setq flags (if (cdr (assoc 70 ent))
                                    (cdr (assoc 70 ent)) 0))
                      (setq onStr (LI:SafeTrim
                                    (LI:GetByHeader vals map "Заморожен")))
                      (if (/= onStr "")
                        (setq flags (LI:SetBit flags 1 (LI:YesNoTrue onStr)))
                      )
                      (setq onStr (LI:SafeTrim
                                    (LI:GetByHeader vals map
                                                    "Заморожен в новых ВЭ")))
                      (if (/= onStr "")
                        (setq flags (LI:SetBit flags 2 (LI:YesNoTrue onStr)))
                      )
                      (setq onStr (LI:SafeTrim
                                    (LI:GetByHeader vals map "Заблокирован")))
                      (if (/= onStr "")
                        (setq flags (LI:SetBit flags 4 (LI:YesNoTrue onStr)))
                      )
                      (setq ent (LI:SetDxf ent 70 flags))
                    )
                    nil
                  )

                  (setq plotStr (LI:SafeTrim
                                  (LI:GetByHeader vals map "Печатается")))
                  (if (/= plotStr "")
                    (setq ent (LI:SetDxf ent 290
                                         (if (LI:YesNoTrue plotStr) 1 0)))
                  )

                  (setq res (vl-catch-all-apply 'entmod (list ent)))
                  (if (LI:IsError res)
                    (progn
                      (LI:AddWarn name "ошибка"
                                  "не удалось изменить слой (entmod)")
                      (princ (strcat "\n[Ошибка]    " (LI:PadRight name 45)
                                     "| не удалось изменить слой"))
                      "Ошибка"
                    )
                    (progn
                      (setq layerObj (LI:AsVla ename))
                      (if layerObj
                        (progn
                          (setq rVal (LI:ToInt (LI:GetByHeader vals map "R")))
                          (setq gVal (LI:ToInt (LI:GetByHeader vals map "G")))
                          (setq bVal (LI:ToInt (LI:GetByHeader vals map "B")))
                          (if (and rVal gVal bVal
                                   (>= rVal 0) (<= rVal 255)
                                   (>= gVal 0) (<= gVal 255)
                                   (>= bVal 0) (<= bVal 255))
                            (progn
                              (if (not (LI:ApplyTrueColor layerObj
                                                          rVal gVal bVal))
                                (LI:AddWarn name "TrueColor"
                                            (strcat "не удалось применить RGB("
                                                    (itoa rVal) ","
                                                    (itoa gVal) ","
                                                    (itoa bVal) ")"))
                              )
                            )
                          )

                          (setq transStr (LI:SafeTrim
                                           (LI:GetByHeader vals map
                                                           "Прозрачность")))
                          (if (/= transStr "")
                            (LI:SetTransparency layerObj transStr)
                          )

                          (setq plotStyle (LI:SafeTrim
                                            (LI:GetByHeader vals map
                                                            "Стиль печати")))
                          (if (/= plotStyle "")
                            (LI:SetPlotStyle layerObj plotStyle)
                          )
                        )
                      )

                      (setq ent2 (entget ename))
                      (setq c62 (cdr (assoc 62 ent2)))
                      (setq f70 (cdr (assoc 70 ent2)))
                      (setq tc2 (cdr (assoc 420 ent2)))
                      (if (null f70) (setq f70 0))

                      (princ (strcat "\n"
                                     (if isNew "[Создан]    " "[Обновлён]  ")
                                     (LI:PadRight name 45)
                                     "| "
                                     (if (and c62 (< c62 0)) "выкл" "вкл ")
                                     " | "
                                     (if (= (logand f70 1) 1) "зам " "разм")
                                     " | "
                                     (if (= (logand f70 4) 4) "блок" "----")
                                     (if (and tc2 (numberp tc2) (>= tc2 0))
                                       (strcat "  RGB("
                                               (itoa (logand (lsh tc2 -16) 255)) ","
                                               (itoa (logand (lsh tc2 -8)  255)) ","
                                               (itoa (logand tc2 255)) ")")
                                       "")))
                      (if isNew "Создан" "Обновлён")
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

;;; ============================================================
;;; Фильтры — вспомогательные
;;; ============================================================

(defun LI:SafeSplit (s delim / tokens pos start lenf)
  (setq s     (LI:ForceString s))
  (setq delim (LI:ForceString delim))
  (if (or (= s "") (= delim ""))
    nil
    (progn
      (setq tokens nil)
      (setq start 0)
      (setq lenf (strlen delim))
      (while (setq pos (vl-string-search delim s start))
        (setq tokens (append tokens
                             (list (LI:SafeTrim
                                     (substr s (1+ start) (- pos start))))))
        (setq start (+ pos lenf))
      )
      (setq tokens (append tokens
                           (list (LI:SafeTrim (substr s (1+ start))))))
      tokens
    )
  )
)

(defun LI:SafeAddUnique (lst s / found x)
  (setq s (LI:SafeTrim s))
  (if (= s "")
    lst
    (progn
      (foreach x lst
        (if (= (strcase (LI:ForceString x)) (strcase s))
          (setq found T)
        )
      )
      (if found lst (append lst (list s)))
    )
  )
)

(defun LI:SafeStrCase (s) (strcase (LI:ForceString s)))

(defun LI:SafeGetFilterDef (defs name / found d)
  (foreach d defs
    (if (and (listp d) (car d)
             (= (LI:SafeStrCase (car d)) (LI:SafeStrCase name)))
      (setq found d)
    )
  )
  found
)

(defun LI:SafeListToComma (lst / s txt x)
  (setq s "")
  (foreach x lst
    (setq txt (LI:SafeTrim x))
    (if (/= txt "")
      (setq s (if (= s "") txt (strcat s "," txt)))
    )
  )
  s
)

(defun LI:SafeGetFilterRef (token / up pos)
  (setq token (LI:SafeTrim token))
  (while (and (> (strlen token) 1)
              (or (= (substr token 1 1) "\"")
                  (= (substr token 1 1) "'")))
    (setq token (LI:SafeTrim (substr token 2)))
  )
  (while (and (> (strlen token) 1)
              (or (= (substr token (strlen token) 1) "\"")
                  (= (substr token (strlen token) 1) "'")))
    (setq token (LI:SafeTrim (substr token 1 (1- (strlen token)))))
  )
  (setq up (strcase token))
  (cond
    ((and (setq pos (vl-string-search "@" token)) (<= pos 5))
      (LI:SafeTrim (substr token (+ pos 2))))
    ((and (setq pos (vl-string-search ">>" token)) (<= pos 5))
      (LI:SafeTrim (substr token (+ pos 3))))
    ((and (setq pos (vl-string-search "FILTER:" up)) (<= pos 5))
      (LI:SafeTrim (substr token (+ pos 8))))
    ((and (setq pos (vl-string-search "ФИЛЬТР:" up)) (<= pos 5))
      (LI:SafeTrim (substr token (+ pos 8))))
    (t nil)
  )
)

(defun LI:ParseFilterDef (raw / tokens direct children token ref)
  (setq tokens (LI:SafeSplit raw ","))
  (setq direct nil)
  (setq children nil)
  (if tokens
    (foreach token tokens
      (setq token (LI:SafeTrim token))
      (if (/= token "")
        (progn
          (setq ref (vl-catch-all-apply 'LI:SafeGetFilterRef (list token)))
          (if (LI:IsError ref) (setq ref nil))
          (cond
            ((and (LI:IsString ref) (/= ref ""))
              (setq children (LI:SafeAddUnique children ref)))
            ((null ref)
              (setq direct (LI:SafeAddUnique direct token)))
            (t
              (if (not (and (LI:IsString ref) (= ref "")))
                (setq direct (LI:SafeAddUnique direct token))
              )
            )
          )
        )
      )
    )
  )
  (cons direct children)
)

(defun LI:FindParsedDef (parsedDefs name / found p)
  (foreach p parsedDefs
    (if (= (LI:SafeStrCase (nth 0 p)) (LI:SafeStrCase name))
      (setq found p)
    )
  )
  found
)

(defun LI:FixedFilterHeaderMap ()
  (list (cons "ИМЯ ФИЛЬТРА" 0) (cons "СПИСОК СЛОЕВ" 1) (cons "ВЫРАЖЕНИЕ" 1))
)

(defun LI:FindFilterHeaderIndex (rows / i row vals found v)
  (setq i 0)
  (foreach row rows
    (if (not found)
      (progn
        (setq vals (LI:GetCellValues row))
        (foreach v vals
          (if (and (not found) (LI:IsString v)
                   (= (strcase (LI:SafeTrim v)) "ИМЯ ФИЛЬТРА"))
            (setq found i)
          )
        )
      )
    )
    (setq i (1+ i))
  )
  found
)

(defun LI:DropRows (lst n / i out)
  (setq i 0)
  (setq out nil)
  (foreach x lst
    (if (>= i n) (setq out (append out (list x))))
    (setq i (1+ i))
  )
  out
)

(defun LI:CreateGroupFilterWithParentCmd
       (name layerString parentName / parentInput)
  (if (or (not name) (= name ""))
    nil
    (progn
      (setq parentInput "")
      (if (and parentName (/= parentName ""))
        (setq parentInput parentName)
      )
      (if (not layerString) (setq layerString ""))
      (command "._-LAYER" "_Filter" "_New" "_Group"
               parentInput layerString name "_Exit" "")
      T
    )
  )
)

(defun LI:DeleteFilterByName (name)
  (if (and name (/= name "")
           (not (LI:IsSystemFilter name)))
    (command "._-LAYER" "_Filter" "_Delete" name "")
  )
)

(defun LI:NameInList (lst name / found x)
  (foreach x lst
    (if (= (LI:SafeStrCase x) (LI:SafeStrCase name))
      (setq found T)
    )
  )
  found
)

(defun LI:FindParentOf (parsedDefs childName / found p name children)
  (setq found nil)
  (foreach p parsedDefs
    (if (not found)
      (progn
        (setq name (nth 0 p))
        (setq children (nth 2 p))
        (if (and (listp children) (LI:NameInList children childName))
          (setq found name)
        )
      )
    )
  )
  found
)

(defun LI:BuildFilterOrder (parsedDefs / remaining ordered createdNames
                             def name parent placed progress)
  (setq remaining parsedDefs)
  (setq ordered nil)
  (setq createdNames nil)
  (setq progress T)
  (while (and remaining progress)
    (setq placed nil)
    (foreach def remaining
      (setq name (nth 0 def))
      (setq parent (LI:FindParentOf parsedDefs name))
      (if (or (null parent) (LI:NameInList createdNames parent))
        (progn
          (setq ordered (append ordered (list def)))
          (setq createdNames (append createdNames (list name)))
          (setq placed T)
        )
      )
    )
    (if placed
      (progn
        (setq remaining nil)
        (foreach def parsedDefs
          (if (not (LI:NameInList createdNames (nth 0 def)))
            (setq remaining (append remaining (list def)))
          )
        )
      )
      (setq progress nil)
    )
  )
  (foreach def remaining
    (setq ordered (append ordered (list def)))
  )
  ordered
)

(defun LI:CollectLayersDeep (parsedDefs name visited
                             / p direct children child sub)
  (if (LI:NameInList visited name)
    nil
    (progn
      (setq p (LI:FindParsedDef parsedDefs name))
      (if (null p)
        nil
        (progn
          (setq direct (nth 1 p))
          (setq children (nth 2 p))
          (if (not (listp direct)) (setq direct nil))
          (foreach child children
            (setq sub (LI:CollectLayersDeep parsedDefs child
                                            (append visited (list name))))
            (foreach l sub
              (setq direct (LI:SafeAddUnique direct l))
            )
          )
          direct
        )
      )
    )
  )
)

;;; ============================================================
;;; Разбор листа «Фильтры»
;;; ============================================================

;;; Читает лист «Фильтры» из XML и возвращает список определений
;;; (имя прямые_слои дочерние_имена). Повторяющиеся строки одного
;;; фильтра склеиваются, как и раньше при импорте.
(defun LI:ParseFilterSheet
       (xml / sheet rows headerIndex headers dataRows
        map row vals name layersList filterDefs uniqueDefs
        def tmp mergedCount parsedDefs p parentName fullLayers
        raw parsed direct children existing)
  (setq filterDefs nil)
  (setq parsedDefs nil)
  (setq sheet (LI:GetWorksheet xml "Фильтры"))
  (if (not sheet)
    (princ "\nЛист 'Фильтры' не найден в XML.")
    (progn
      (setq rows (LI:GetRows sheet))
      (princ (strcat "\nНайдено строк на листе 'Фильтры': "
                     (itoa (length rows))))
      (if (< (length rows) 2)
        (princ "\nВ файле нет данных о фильтрах.")
        (progn
          (setq headerIndex (LI:FindFilterHeaderIndex rows))
          (if headerIndex
            (progn
              (setq headers (LI:GetCellValues (nth headerIndex rows)))
              (setq dataRows (LI:DropRows rows (1+ headerIndex)))
              (setq map (LI:BuildHeaderMap headers))
              (if (or (not (assoc "ИМЯ ФИЛЬТРА" map))
                      (not (assoc "СПИСОК СЛОЕВ" map)))
                (setq map (LI:FixedFilterHeaderMap))
              )
            )
            (progn
              (setq dataRows rows)
              (setq map (LI:FixedFilterHeaderMap))
            )
          )
          (foreach row dataRows
            (setq vals (LI:GetCellValues row))
            (setq name (LI:SafeTrim (LI:GetByHeader vals map "Имя фильтра")))
            (setq layersList (LI:ForceString
                               (LI:GetByHeader vals map "Список слоев")))
            (if (= layersList "")
              (setq layersList (LI:ForceString
                                 (LI:GetByHeader vals map "Выражение")))
            )
            (if (and (/= name "")
                     (/= (LI:SafeStrCase name) "ИМЯ ФИЛЬТРА")
                     (/= (LI:SafeStrCase name) "ФИЛЬТРЫ НЕ НАЙДЕНЫ"))
              (setq filterDefs (append filterDefs
                                       (list (list name layersList))))
            )
          )
          (princ (strcat "\nСобрано строк: " (itoa (length filterDefs))))

          ;; ---------- Объединение строк с одинаковым именем ----------
          ;; В Excel-таблице один фильтр может быть описан несколькими
          ;; строками: одна — прямые слои, остальные — ссылки >>Имя.
          ;; Склеиваем их "Список слоев" через запятую, чтобы получить
          ;; полное определение в одном месте.
          (setq uniqueDefs nil)
          (setq mergedCount 0)
          (foreach def filterDefs
            (setq name       (LI:ForceString (car def)))
            (setq layersList (LI:ForceString (cadr def)))
            (setq existing   (LI:SafeGetFilterDef uniqueDefs name))
            (if existing
              (progn
                (setq tmp nil)
                (foreach u uniqueDefs
                  (if (and (listp u) (car u)
                           (= (LI:SafeStrCase (car u))
                              (LI:SafeStrCase name)))
                    (setq tmp
                      (append tmp
                              (list (list (car u)
                                          (if (and (cadr u)
                                                   (/= (cadr u) ""))
                                            (strcat (cadr u) "," layersList)
                                            layersList)))))
                    (setq tmp (append tmp (list u)))
                  )
                )
                (setq uniqueDefs tmp)
                (setq mergedCount (1+ mergedCount))
              )
              (setq uniqueDefs (append uniqueDefs (list def)))
            )
          )
          (setq filterDefs uniqueDefs)
          (princ (strcat "\nНайдено фильтров: " (itoa (length filterDefs))))
          (if (> mergedCount 0)
            (princ (strcat "  (объединено строк: " (itoa mergedCount) ")"))
          )

          (setq parsedDefs nil)
          (foreach def filterDefs
            (setq name (LI:ForceString (car def)))
            (setq raw  (LI:ForceString (cadr def)))
            (setq parsed (vl-catch-all-apply 'LI:ParseFilterDef (list raw)))
            (if (LI:IsError parsed) (setq parsed (cons nil nil)))
            (if (not (listp parsed)) (setq parsed (cons nil nil)))
            (setq direct   (car parsed))
            (setq children (cdr parsed))
            (if (not (listp direct))   (setq direct nil))
            (if (not (listp children)) (setq children nil))
            (setq parsedDefs
              (append parsedDefs (list (list name direct children))))
          )
          (princ "\n\n")
          (princ "\nСтруктура фильтров:")
          (princ (strcat "\n  "
                         (LI:PadRight "Фильтр" 24) "| "
                         (LI:PadRight "Родитель" 14)
                         "| Слои (с вложенными)"))
          (princ (strcat "\n  "
                         (LI:RepeatChar "-" 24) "+"
                         (LI:RepeatChar "-" 15) "+"
                         (LI:RepeatChar "-" 46)))
          (foreach p parsedDefs
            (setq name (nth 0 p))
            (setq parentName (LI:FindParentOf parsedDefs name))
            (if (null parentName) (setq parentName "-"))
            (setq fullLayers (LI:CollectLayersDeep parsedDefs name nil))
            (princ (strcat "\n  "
                           (LI:PadRight name 24) "| "
                           (LI:PadRight parentName 14) "| "
                           (LI:SafeListToComma fullLayers)))
          )
          (princ "\n\n")
        )
      )
    )
  )
  parsedDefs
)

;;; ============================================================
;;; Импорт фильтров
;;; ============================================================

(defun LI:ImportFilters
       (doc xml / parsedDefs orderedDefs p name fullLayers
        parentName directString reversedDefs delNames createdNames
        delPass ok parentLabel layerCount sheet rows
        curTree legacyInfo legacyLayers ourLayers nm rec)
  (princ "\n\n")
  (princ "\nЧтение фильтров слоёв...")
  (setq *LI:AR-LAST-COUNT* nil)
  (setq *LI:AR-LAST-XREF* 0)
  (setq *LI:UNASSIGNED-LAST-COUNT* nil)
  (setq *LI:UNASSIGNED-LAST-XREF* 0)
  (setq sheet (LI:GetWorksheet xml "Фильтры"))
  (setq rows (if sheet (LI:GetRows sheet) nil))
  (if (not sheet)
    (princ "\nЛист 'Фильтры' не найден в XML.")
    (if (< (length rows) 2)
      (princ "\nВ файле нет данных о фильтрах.")
      (progn
        (setq parsedDefs (LI:ParseFilterSheet xml))
        (setq orderedDefs (LI:BuildFilterOrder parsedDefs))
        ;; Снимок старых фильтров и слоёв нашего шаблона — до удаления.
        (setq ourLayers (LI:ARF-TemplateLayers parsedDefs))
        (setq curTree (LI:FilterTreeFull (LI:FilterRootDictData) nil))
        (setq legacyInfo (LI:LegacyInfo curTree ourLayers))
        (setq legacyLayers (LI:LegacyLayers curTree))
        (princ "\nЭтап: удаление старых и одноимённых фильтров...")
        (setq reversedDefs nil)
        (foreach p orderedDefs
          (setq reversedDefs (cons p reversedDefs))
        )
        (setq delNames nil)
        (foreach p reversedDefs
          (setq delNames (LI:SafeAddUnique delNames (nth 0 p)))
        )
        (foreach name *LI:OLD-FILTERS*
          (setq delNames (LI:SafeAddUnique delNames name))
        )
        ;; Удалить все пользовательские фильтры реципиента.
        (if (and *LI:DELETE-ALL-FILTERS* parsedDefs)
          (foreach rec curTree
            (setq nm (nth 0 rec))
            (if (not (LI:IsSystemFilter nm))
              (setq delNames (LI:SafeAddUnique delNames nm))
            )
          )
        )
        (setq delPass 1)
        (while (<= delPass 3)
          (foreach name delNames
            (LI:DeleteFilterByName name)
          )
          (setq delPass (1+ delPass))
        )
        (princ " готово.")
        (princ "\n\n")
        (princ "\nСоздание фильтров:")
        (princ (strcat "\n  "
                       (LI:PadRight "Статус" 9) "| "
                       (LI:PadRight "Фильтр" 24) "| "
                       (LI:PadRight "Родитель" 14) "| Слои"))
        (princ (strcat "\n  "
                       (LI:RepeatChar "-" 9)  "+"
                       (LI:RepeatChar "-" 25) "+"
                       (LI:RepeatChar "-" 15) "+"
                       (LI:RepeatChar "-" 12)))
        (setq createdNames nil)
        (foreach p orderedDefs
          (setq name (nth 0 p))
          (setq parentName (LI:FindParentOf parsedDefs name))
          (if (null parentName) (setq parentName ""))
          (setq fullLayers (LI:CollectLayersDeep parsedDefs name nil))
          (setq layerCount (length fullLayers))
          (setq directString (LI:SafeListToComma fullLayers))
          (if (and (/= parentName "")
                   (not (LI:NameInList createdNames parentName)))
            (progn
              (LI:AddWarn name "фильтр"
                          (strcat "родитель '" parentName
                                  "' не найден, создан как корневой"))
              (setq parentName "")
            )
          )
          (setq parentLabel (if (= parentName "") "-" parentName))
          (setq ok (LI:CreateGroupFilterWithParentCmd
                     name directString parentName))
          (princ (strcat "\n  "
                         (LI:PadRight (if ok "[OK] " "[!] ") 9) "| "
                         (LI:PadRight name 24) "| "
                         (LI:PadRight parentLabel 14) "| "
                         (itoa layerCount)
                         (if (= layerCount 1) " слой" " слоёв")))
          (if ok
            (setq createdNames (append createdNames (list name)))
            (LI:AddWarn name "фильтр" "не удалось создать")
          )
        )
        (princ "\n\n")
        (vl-catch-all-apply 'vl-cmdf (list "._REGENALL"))
        ;; ---------- АР: все слои вне шаблона ----------
        (if (and *LI:AUTO-AR* parsedDefs)
          (setq *LI:AR-LAST-COUNT* (LI:ARF-RunFromDefs parsedDefs))
          nil
        )
        ;; ---------- Нераспределённые: старые фильтры вне шаблона ----------
        (if (and *LI:AUTO-UNASSIGNED* parsedDefs)
          (progn
            (LI:PrintLegacyCheck legacyInfo)
            (setq *LI:UNASSIGNED-LAST-COUNT*
                  (LI:RunUnassigned parsedDefs legacyLayers))
          )
          nil
        )
        (princ "\nИмпорт фильтров завершён.")
      )
    )
  )
  (princ)
)

;;; ============================================================
;;; Импорт из файла
;;; ============================================================

(defun LI:ImportFromFile
       (fname / doc xml sheet rows headers map vals
        status created updated errors skipped oldLayer
        parsedRows rec name vals2)
  (setq created 0)
  (setq updated 0)
  (setq errors  0)
  (setq skipped 0)
  (setq *LI:WARN* nil)
  (setq xml (LI:ReadFile fname))
  (if (not xml)
    (princ (strcat "\nНе удалось прочитать файл: " fname))
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq oldLayer (getvar "CLAYER"))
      (vl-catch-all-apply 'setvar (list "CLAYER" "0"))
      (setq sheet (LI:GetWorksheet xml "Слои"))
      (if (not sheet) (setq sheet (LI:GetFirstWorksheet xml)))
      (if (not sheet)
        (princ "\nНе найден лист слоёв в XML-файле.")
        (progn
          (setq rows (LI:GetRows sheet))
          (if (< (length rows) 2)
            (princ "\nВ файле нет данных слоёв.")
            (progn
              (setq headers (LI:GetCellValues (car rows)))
              (setq map (LI:BuildHeaderMap headers))
              (setq parsedRows nil)
              (foreach row (cdr rows)
                (setq vals (LI:GetCellValues row))
                (if vals
                  (progn
                    (setq name (LI:SafeTrim
                                 (LI:GetByHeader vals map "Имя слоя")))
                    (setq parsedRows
                      (append parsedRows (list (cons name vals))))
                  )
                )
              )
              (setq parsedRows
                (vl-sort parsedRows
                  '(lambda (a b)
                     (< (strcase (car a)) (strcase (car b))))
                )
              )
              (foreach rec parsedRows
                (setq vals2 (cdr rec))
                (if vals2
                  (progn
                    (setq status (LI:ApplyLayerRow doc vals2 map))
                    (cond
                      ((= status "Создан")   (setq created (1+ created)))
                      ((= status "Обновлён")(setq updated (1+ updated)))
                      ((= status "Ошибка")   (setq errors  (1+ errors)))
                      ((= status "Пропущен")(setq skipped (1+ skipped)))
                    )
                  )
                )
              )
            )
          )
        )
      )

      (LI:PrintWarnings)
      (LI:ImportFilters doc xml)

      (if (and oldLayer (tblsearch "LAYER" oldLayer))
        (vl-catch-all-apply 'setvar (list "CLAYER" oldLayer))
      )

      (princ "\n\n")
      (princ "\nГотово.")
      (princ (strcat "\nСоздано слоёв:   " (itoa created)))
      (princ (strcat "\nОбновлено слоёв: " (itoa updated)))
      (princ (strcat "\nОшибок:          " (itoa errors)))
      (if (> skipped 0)
        (princ (strcat "\nПропущено (XREF): " (itoa skipped)))
      )
      (if (and *LI:AUTO-AR* *LI:AR-LAST-COUNT*)
        (princ (strcat "\nФильтр АР: " (itoa *LI:AR-LAST-COUNT*)
                       " слоёв (XREF пропущено: "
                       (itoa *LI:AR-LAST-XREF*) ")"))
      )
      (if (and *LI:AUTO-UNASSIGNED* *LI:UNASSIGNED-LAST-COUNT*)
        (princ (strcat "\nНераспределённые: " (itoa *LI:UNASSIGNED-LAST-COUNT*)
                       " слоёв (из старых фильтров)"))
      )

      (princ "\n\n")
      (princ "\n=== Применение переменных слоёв ===")
      (C:МОИСЛОИПОУМОЛЧАНИЮ)
    )
  )
  (princ)
)

;;; ============================================================
;;; Поиск шаблона
;;; ============================================================

(defun LI:EnsureSlash (p)
  (if (and p (/= p ""))
    (if (= (substr p (strlen p) 1) "\\")
      p
      (strcat p "\\")
    )
    p
  )
)

(defun LI:DrawingDir (/ p)
  (setq p (getvar "DWGPREFIX"))
  (if (and p (/= p ""))
    (if (vl-file-directory-p p)
      (setq p (LI:EnsureSlash p))
      (setq p (LI:EnsureSlash (vl-filename-directory p)))
    )
    (setq p nil)
  )
  (if (or (not p) (= p "") (= p "\\")) nil p)
)

(defun LI:LocalTemplateFile (/ dir base candidates f c result)
  (setq dir (LI:DrawingDir))
  (if dir
    (progn
      (setq base (getvar "DWGNAME"))
      (if (or (not base) (= base "")) (setq base "Untitled.dwg"))
      (setq base (vl-filename-base base))
      (setq candidates nil)
      ;; 1) основной шаблон из настроек
      (if (and *LI:TEMPLATE-NAME*
               (= (type *LI:TEMPLATE-NAME*) 'STR)
               (/= *LI:TEMPLATE-NAME* ""))
        (setq candidates
          (append candidates (list (strcat dir *LI:TEMPLATE-NAME*))))
      )
      ;; 2) запасные имена
      (foreach f *LI:TEMPLATE-FALLBACKS*
        (if (and f (= (type f) 'STR) (/= f ""))
          (setq candidates
            (append candidates (list (strcat dir f))))
        )
      )
      ;; 3) прежние варианты
      (setq candidates
        (append candidates
          (list
            (strcat dir "Слои и фильтры.xml")
            (strcat dir base " Слои.xml")
            (strcat dir base ".xml")
            (strcat dir "Слои.xml")
          )
        )
      )
      ;; убрать повторы, сохранив порядок
      (setq c nil)
      (foreach f candidates
        (setq c (LI:SafeAddUnique c f))
      )
      (setq candidates c)
      (princ (strcat "\n[Поиск] Папка чертежа: " dir))
      (princ (strcat "\n[Поиск] Имя чертежа (без расширения): "
                     (vl-filename-base (getvar "DWGNAME"))))
      (foreach f candidates
        (princ (strcat "\n[Поиск] Проверка: " f))
        (if (and (not result) (findfile f))
          (progn
            (setq result f)
            (princ " -> НАЙДЕН!")
          )
        )
      )
      result
    )
    nil
  )
)

(defun LI:FindTemplateFile (/ f n)
  (setq f (LI:LocalTemplateFile))
  (if (and (not f)
           *LI:TEMPLATE-NAME*
           (= (type *LI:TEMPLATE-NAME*) 'STR)
           (/= *LI:TEMPLATE-NAME* ""))
    (setq f (findfile *LI:TEMPLATE-NAME*))
  )
  (foreach n *LI:TEMPLATE-FALLBACKS*
    (if (and (not f) n (= (type n) 'STR) (/= n ""))
      (setq f (findfile n))
    )
  )
  (if (not f) (setq f (findfile "Слои и фильтры.xml")))
  (if (not f) (setq f (findfile "Слои.xml")))
  f
)

(defun LI:SelectTemplateFile (/ initial f)
  (setq initial (LI:FindTemplateFile))
  (if (or (not initial) (not (eq (type initial) 'STR)))
    (setq initial (LI:DrawingDir))
  )
  (if (or (not initial) (not (eq (type initial) 'STR)))
    (setq initial "")
  )
  (setq f (getfiled "Выберите XML-шаблон слоёв" initial "xml" 0))
  f
)

(defun LI:Run (/ fname)
  (setq fname (LI:FindTemplateFile))
  (if (and fname (not *LI:ALWAYS-ASK*))
    (princ (strcat "\nИспользуется шаблон: " fname))
    (progn
      (if (not fname)
        (princ "\nШаблон слоёв в папке чертежа не найден."))
      (setq fname (LI:SelectTemplateFile))
    )
  )
  (if (and fname (eq (type fname) 'STR) (/= fname ""))
    (LI:ImportFromFile fname)
    (princ "\nФайл шаблона не выбран.")
  )
  (princ)
)

;;; ============================================================
;;; Импорт ТОЛЬКО фильтров (без слоёв).
;;; Читает XML-шаблон и вызывает LI:ImportFilters напрямую.
;;; Слои, переменные слоёв, содержимое чертежа не трогаются.
;;; ============================================================

(defun LI:RunFilters (/ fname xml)
  (setq fname (LI:FindTemplateFile))
  (if (and fname (not *LI:ALWAYS-ASK*))
    (princ (strcat "\nИспользуется шаблон: " fname))
    (progn
      (if (not fname)
        (princ "\nШаблон слоёв в папке чертежа не найден."))
      (setq fname (LI:SelectTemplateFile))
    )
  )
  (if (and fname (eq (type fname) 'STR) (/= fname ""))
    (progn
      (setq *LI:WARN* nil)
      (setq xml (LI:ReadFile fname))
      (if (not xml)
        (princ (strcat "\nНе удалось прочитать файл: " fname))
        (progn
          (LI:ImportFilters nil xml)
          (LI:PrintWarnings)
          (princ "\n")
          (princ (strcat "\nГотово. Фильтры импортированы из: " fname))
        )
      )
    )
    (princ "\nФайл шаблона не выбран.")
  )
  (princ)
)

(defun C:МОИСЛОИФИЛЬТРЫ () (LI:RunFilters))

(defun C:МОИСЛОИЗАГРУЗИТЬ () (LI:Run))

;;; ============================================================
;;; Очистка фильтров
;;; ============================================================

(defun C:МОИСЛОИФИЛЬТРЫОЧИСТИТЬ
       (/ fname xml sheet rows vals name names filtered pass)
  (princ "\n=== Удаление фильтров слоёв ===")
  (setq names nil)
  (setq fname (LI:FindTemplateFile))
  (if fname
    (progn
      (princ (strcat "\nШаблон: " fname))
      (setq xml (LI:ReadFile fname))
      (if xml
        (progn
          (setq sheet (LI:GetWorksheet xml "Фильтры"))
          (if sheet
            (progn
              (setq rows (LI:GetRows sheet))
              (foreach row (cdr rows)
                (setq vals (LI:GetCellValues row))
                (if vals
                  (progn
                    (setq name (LI:SafeTrim (car vals)))
                    (if (and name (/= name "")
                                (/= (LI:SafeStrCase name) "ИМЯ ФИЛЬТРА")
                                (/= (LI:SafeStrCase name)
                                    "ФИЛЬТРЫ НЕ НАЙДЕНЫ"))
                      (setq names (LI:SafeAddUnique names name))
                    )
                  )
                )
              )
              (setq names (reverse names))
            )
            (princ "\nЛист 'Фильтры' не найден.")
          )
        )
        (princ "\nНе удалось прочитать XML.")
      )
    )
    (princ "\nШаблон не найден в папке чертежа.")
  )
  (foreach name *LI:OLD-FILTERS*
    (setq names (LI:SafeAddUnique names name))
  )
  ;; Системные фильтры не удаляем ни при каких условиях.
  (setq filtered nil)
  (foreach name names
    (if (not (LI:IsSystemFilter name))
      (setq filtered (LI:SafeAddUnique filtered name))
    )
  )
  (setq names filtered)
  (if (null names)
    (progn
      (setq name (getstring T
        "\nИмена фильтров через запятую (Enter — отмена): "))
      (if (and name (/= name ""))
        (setq names (LI:SafeSplit name ","))
      )
    )
  )
  (if names
    (progn
      (princ (strcat "\nК удалению: " (itoa (length names))))
      (setq pass 1)
      (while (<= pass 3)
        (princ (strcat "\n--- Проход " (itoa pass) " ---"))
        (foreach name names
          (if (and name (/= name ""))
            (progn
              (princ (strcat "\n  " name))
              (LI:DeleteFilterByName name)
            )
          )
        )
        (setq pass (1+ pass))
      )
    )
    (princ "\nСписок фильтров пуст — нечего удалять.")
  )
  ;; Дополнительно удаляем служебные фильтры (они не входят в шаблон).
  (if *LI:AR-CLEAN-WITH-FILTERS*
    (progn
      (princ (strcat "\nУдаление фильтра '" *LI:AR-FILTER-NAME* "'..."))
      (setq pass 1)
      (while (<= pass 2)
        (LI:DeleteFilterByName *LI:AR-FILTER-NAME*)
        (setq pass (1+ pass))
      )
      (princ (strcat "\nУдаление фильтра '"
                     *LI:UNASSIGNED-FILTER-NAME* "'..."))
      (setq pass 1)
      (while (<= pass 2)
        (LI:DeleteFilterByName *LI:UNASSIGNED-FILTER-NAME*)
        (setq pass (1+ pass))
      )
    )
  )
  (princ "\nГотово.")
  (princ)
)

;;; ============================================================
;;; Проверка LISP-файла (скобки и строки)
;;; ============================================================

(defun LI:ReadTextFile (fname / f line out)
  (setq out "")
  (if (setq f (open fname "r"))
    (progn
      (while (setq line (read-line f))
        (setq out (strcat out line "\n"))
      )
      (close f)
      out
    )
    nil
  )
)

;;; Проверяет баланс скобок и кавычек с учётом ;-комментариев.
;;; Возвращает T, если ошибок нет.
(defun LI:CheckFileText (txt / i n ch depth line inStr strLine inComment
                            badDepth badLine ok)
  (setq i 0)
  (setq n (strlen txt))
  (setq depth 0)
  (setq line 1)
  (setq inStr nil)
  (setq inComment nil)
  (setq strLine 0)
  (setq badDepth nil)
  (setq badLine nil)
  (while (< i n)
    (setq ch (substr txt (1+ i) 1))
    (cond
      (inStr
        (cond
          ((= ch "\\") (setq i (+ i 2)))
          ((= ch "\"") (setq inStr nil) (setq i (1+ i)))
          (t
            (if (= ch "\n") (setq line (1+ line)))
            (setq i (1+ i))
          )
        )
      )
      (inComment
        (if (= ch "\n")
          (progn (setq inComment nil) (setq line (1+ line)))
        )
        (setq i (1+ i))
      )
      (t
        (cond
          ((= ch ";") (setq inComment T) (setq i (1+ i)))
          ((= ch "\"") (setq inStr T) (setq strLine line) (setq i (1+ i)))
          ((= ch "(") (setq depth (1+ depth)) (setq i (1+ i)))
          ((= ch ")")
            (setq depth (1- depth))
            (if (and (null badDepth) (< depth 0))
              (progn
                (setq badDepth depth)
                (setq badLine line)
              )
            )
            (setq i (1+ i))
          )
          ((= ch "\n") (setq line (1+ line)) (setq i (1+ i)))
          (t (setq i (1+ i)))
        )
      )
    )
  )
  (setq ok T)
  (if inStr
    (progn
      (setq ok nil)
      (princ (strcat "\n[FAIL] Незакрытая строка, начатая в строке "
                     (itoa strLine) "."))
    )
  )
  (if badDepth
    (progn
      (setq ok nil)
      (princ (strcat "\n[FAIL] Лишняя закрывающая скобка в строке "
                     (itoa badLine) "."))
    )
  )
  (if (> depth 0)
    (progn
      (setq ok nil)
      (princ (strcat "\n[FAIL] Не закрыто скобок: " (itoa depth)
                     " (итоговая глубина " (itoa depth) ")."))
    )
  )
  (if ok
    (princ (strcat "\n[OK] Скобки и строки сбалансированы (строк: "
                   (itoa line) ", глубина 0)."))
    (princ (strcat "\nСтрок в файле: " (itoa line) "."))
  )
  ok
)

(defun C:МОИСЛОИПРОВЕРКАФАЙЛА (/ f txt)
  (princ "\n\n=== Проверка файла AutoLISP ===")
  (setq f (getfiled "Проверить LISP-файл" "" "lsp" 0))
  (if (and f (/= f ""))
    (progn
      (princ (strcat "\nФайл: " f))
      (setq txt (LI:ReadTextFile f))
      (if txt
        (LI:CheckFileText txt)
        (princ "\n[FAIL] Не удалось прочитать файл.")
      )
    )
    (princ "\n[Инфо] Файл не выбран.")
  )
  (princ "\n")
  (princ)
)

;;; ============================================================
;;; СЕКЦИЯ 4: фильтр АР (всё, что не входит в фильтры шаблона)
;;; ============================================================

;;; Все слои чертежа, отсортированные по имени без учёта регистра.
(defun LI:ARF-AllLayers (/ doc layers lyr name out)
  (setq out nil)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq layers (vl-catch-all-apply 'vla-get-Layers (list doc)))
  (if (not (LI:IsError layers))
    (vlax-for lyr layers
      (setq name (vl-catch-all-apply 'vla-get-Name (list lyr)))
      (if (and (not (LI:IsError name))
               (LI:IsString name)
               (/= name ""))
        (setq out (LI:SafeAddUnique out name))
      )
    )
  )
  (vl-sort out
    '(lambda (a b) (< (strcase a) (strcase b))))
)

;;; Слой внешней ссылки: бит 16 кода 70.
(defun LI:ARF-IsXref (name / en ent flags)
  (setq name (LI:SafeTrim name))
  (setq en (if (= name "") nil (tblobjname "LAYER" name)))
  (if en
    (progn
      (setq ent (entget en))
      (setq flags (cdr (assoc 70 ent)))
      (and (numberp flags) (= 16 (logand flags 16)))
    )
    nil
  )
)

;;; Системный слой, который не включается в АР.
(defun LI:ARF-IsSkip (name)
  (LI:NameInList *LI:AR-SKIP-LAYERS* name)
)

;;; Объединение слоёв всех фильтров шаблона (с вложенными).
(defun LI:ARF-TemplateLayers (parsedDefs / out def lyr)
  (setq out nil)
  (foreach def parsedDefs
    (foreach lyr (LI:CollectLayersDeep parsedDefs (nth 0 def) nil)
      (setq out (LI:SafeAddUnique out lyr))
    )
  )
  out
)

;;; Полный расчёт: (слои xref системных шаблонных).
;;; Имя целевого фильтра — *LI:ARF-NAME-OVERRIDE* или *LI:AR-FILTER-NAME*.
(defun LI:ARF-Compute (parsedDefs
                       / all templ out name xrefCnt sysCnt usedCnt fname)
  (setq fname (if (and *LI:ARF-NAME-OVERRIDE*
                       (= (type *LI:ARF-NAME-OVERRIDE*) 'STR)
                       (/= *LI:ARF-NAME-OVERRIDE* ""))
                *LI:ARF-NAME-OVERRIDE*
                *LI:AR-FILTER-NAME*))
  (setq all (LI:ARF-AllLayers))
  (setq templ (LI:ARF-TemplateLayers parsedDefs))
  (setq out nil)
  (setq xrefCnt 0)
  (setq sysCnt 0)
  (setq usedCnt 0)
  (foreach name all
    (cond
      ((and *LI:AR-SKIP-XREF* (LI:ARF-IsXref name))
        (setq xrefCnt (1+ xrefCnt))
      )
      ((LI:ARF-IsSkip name)
        (setq sysCnt (1+ sysCnt))
      )
      ((LI:NameInList templ name)
        (setq usedCnt (1+ usedCnt))
      )
      ((LI:NameInList (list fname) name)
        nil
      )
      (t
        (setq out (append out (list name)))
      )
    )
  )
  (list out xrefCnt sysCnt usedCnt)
)

;;; Итоговый список слоёв АР.
(defun LI:ARF-BuildList (parsedDefs)
  (car (LI:ARF-Compute parsedDefs))
)

;;; Создание фильтра в корне дерева (идемпотентно).
;;; Имя фильтра — *LI:ARF-NAME-OVERRIDE* или *LI:AR-FILTER-NAME*.
(defun LI:ARF-Create (layers / layerString pass fname)
  (setq fname (if (and *LI:ARF-NAME-OVERRIDE*
                       (= (type *LI:ARF-NAME-OVERRIDE*) 'STR)
                       (/= *LI:ARF-NAME-OVERRIDE* ""))
                *LI:ARF-NAME-OVERRIDE*
                *LI:AR-FILTER-NAME*))
  (if (null layers)
    nil
    (progn
      (setq layerString (LI:SafeListToComma layers))
      (setq pass 1)
      (while (<= pass 2)
        (LI:DeleteFilterByName fname)
        (setq pass (1+ pass))
      )
      (LI:CreateGroupFilterWithParentCmd fname layerString "")
      T
    )
  )
)

;;; Полная таблица слоёв фильтра и арифметика отбора.
;;; Имя фильтра — *LI:ARF-NAME-OVERRIDE* или *LI:AR-FILTER-NAME*.
(defun LI:ARF-Report (layers skippedXref skippedSys
                      / all total nonXref arLayer templCnt name fname)
  (setq fname (if (and *LI:ARF-NAME-OVERRIDE*
                       (= (type *LI:ARF-NAME-OVERRIDE*) 'STR)
                       (/= *LI:ARF-NAME-OVERRIDE* ""))
                *LI:ARF-NAME-OVERRIDE*
                *LI:AR-FILTER-NAME*))
  (setq all (LI:ARF-AllLayers))
  (setq total (length all))
  (setq nonXref (- total skippedXref))
  (setq arLayer (if (LI:NameInList all fname) 1 0))
  (setq templCnt (- nonXref skippedSys (length layers) arLayer))
  (princ "\n\n")
  (princ (strcat "\n=== Фильтр " fname " ==="))
  (princ (strcat "\n  "
                 (LI:PadRight "Слой" 45) "| Статус"))
  (princ (strcat "\n  "
                 (LI:RepeatChar "-" 45) "+"
                 (LI:RepeatChar "-" 10)))
  (foreach name layers
    (princ (strcat "\n  "
                   (LI:PadRight name 45) "| [OK]"))
  )
  (if (null layers)
    (princ "\n  [Инфо] Список пуст.")
  )
  (princ "\n")
  (princ (strcat "\n  Слоёв в " fname ": " (itoa (length layers))))
  (princ (strcat "\n  Пропущено XREF: " (itoa skippedXref)))
  (princ (strcat "\n  Пропущено системных: " (itoa skippedSys)))
  (princ (strcat "\n  Всего не-XREF слоёв: " (itoa nonXref)))
  (princ (strcat "\n  Слоёв в фильтрах-источнике: " (itoa templCnt)))
  (princ (strcat "\n  Проверка: " (itoa nonXref) " - "
                 (itoa skippedSys) " - " (itoa templCnt)
                 " = " (itoa (+ (length layers) arLayer))))
  (princ "\n\n")
)

;;; Определения фильтров из реального дерева чертежа (fallback).
(defun LI:ARF-DrawingDefs (/ tree out rec name)
  (setq tree (LI:FilterTreeFull (LI:FilterRootDictData) nil))
  (foreach rec tree
    (setq name (nth 0 rec))
    (if (and (not (LI:IsSystemFilter name))
             (not (LI:NameInList (list *LI:AR-FILTER-NAME*) name))
             (not (LI:NameInList (list *LI:UNASSIGNED-FILTER-NAME*) name)))
      (setq out (append out (list (list name (nth 1 rec) (nth 2 rec)))))
    )
  )
  out
)

;;; Построение и отчёт по готовым определениям фильтров.
(defun LI:ARF-RunFromDefs (parsedDefs / res layers xrefCnt sysCnt)
  (setq res (LI:ARF-Compute parsedDefs))
  (setq layers  (nth 0 res))
  (setq xrefCnt (nth 1 res))
  (setq sysCnt  (nth 2 res))
  (setq *LI:AR-LAST-COUNT* (length layers))
  (setq *LI:AR-LAST-XREF*  xrefCnt)
  (if (null layers)
    (progn
      (princ (strcat "\n[Инфо] Список АР пуст — фильтр '"
                     *LI:AR-FILTER-NAME* "' не создаётся: нет не-XREF слоёв вне фильтров шаблона."))
      (LI:ARF-Report layers xrefCnt sysCnt)
      nil
    )
    (if (LI:ARF-Create layers)
      (progn
        (princ (strcat "\n[OK] Фильтр '" *LI:AR-FILTER-NAME*
                       "' создан: " (itoa (length layers)) " слоёв."))
        (LI:ARF-Report layers xrefCnt sysCnt)
        (length layers)
      )
      (progn
        (princ "\n[Ошибка] Не удалось создать фильтр АР.")
        (LI:ARF-Report layers xrefCnt sysCnt)
        nil
      )
    )
  )
)

;;; Совпадение имени старого фильтра с масками (подстрока, без регистра).
(defun LI:MatchesLegacyFilter (name / up m found)
  (setq up (strcase (LI:SafeTrim name)))
  (foreach m *LI:LEGACY-FILTER-MASKS*
    (if (and (not found) m (= (type m) 'STR) (/= m "")
             (vl-string-search (strcase m) up))
      (setq found T)
    )
  )
  found
)

;;; Снимок старых фильтров: (имя всего наших нераспределённых).
;;; ourLayers — слои фильтров шаблона.
(defun LI:LegacyInfo (tree ourLayers / out rec name lls matched
                        unmatched l)
  (setq out nil)
  (foreach rec tree
    (setq name (nth 0 rec))
    (if (LI:MatchesLegacyFilter name)
      (progn
        (setq lls (nth 3 rec))
        (setq matched 0)
        (setq unmatched 0)
        (foreach l lls
          (cond
            ((LI:NameInList ourLayers l)
              (setq matched (1+ matched))
            )
            ((and *LI:AR-SKIP-XREF* (LI:ARF-IsXref l))
              nil
            )
            ((LI:ARF-IsSkip l)
              nil
            )
            (t
              (setq unmatched (1+ unmatched))
            )
          )
        )
        (setq out
          (append out
                  (list (list name (length lls) matched unmatched))))
      )
    )
  )
  out
)

;;; Объединение слоёв старых фильтров (по маскам).
(defun LI:LegacyLayers (tree / out rec name l)
  (setq out nil)
  (foreach rec tree
    (setq name (nth 0 rec))
    (if (LI:MatchesLegacyFilter name)
      (foreach l (nth 3 rec)
        (setq out (LI:SafeAddUnique out l))
      )
    )
  )
  out
)

;;; Таблица сверки старых фильтров.
(defun LI:PrintLegacyCheck (legacyInfo / rec)
  (if legacyInfo
    (progn
      (princ "\n\n")
      (princ "\n=== Сверка старых фильтров ===")
      (princ (strcat "\n  "
                     (LI:PadRight "Старый фильтр" 40) "| "
                     (LI:PadRight "Слоёв" 6) "| "
                     (LI:PadRight "В наших" 8) "| "
                     "Нераспределённые"))
      (princ (strcat "\n  "
                     (LI:RepeatChar "-" 40) "+"
                     (LI:RepeatChar "-" 7) "+"
                     (LI:RepeatChar "-" 9) "+"
                     (LI:RepeatChar "-" 17)))
      (foreach rec legacyInfo
        (princ (strcat "\n  "
                       (LI:PadRight (nth 0 rec) 40) "| "
                       (LI:PadRight (itoa (nth 1 rec)) 6) "| "
                       (LI:PadRight (itoa (nth 2 rec)) 8) "| "
                       (itoa (nth 3 rec))))
      )
      (princ "\n\n")
    )
  )
)

;;; Создание «Нераспределённые»: слои старых фильтров,
;;; не входящие в структуру шаблона (минус XREF и системные).
(defun LI:RunUnassigned (parsedDefs legacyLayers
                         / templ layers l name xrefCnt sysCnt)
  (setq *LI:ARF-NAME-OVERRIDE* *LI:UNASSIGNED-FILTER-NAME*)
  (setq templ (LI:ARF-TemplateLayers parsedDefs))
  (setq layers nil)
  (setq xrefCnt 0)
  (setq sysCnt 0)
  (foreach l legacyLayers
    (cond
      ((LI:NameInList templ l)
        nil
      )
      ((and *LI:AR-SKIP-XREF* (LI:ARF-IsXref l))
        (setq xrefCnt (1+ xrefCnt))
      )
      ((LI:ARF-IsSkip l)
        (setq sysCnt (1+ sysCnt))
      )
      ((LI:NameInList (list *LI:UNASSIGNED-FILTER-NAME*) l)
        nil
      )
      (t
        (setq layers (append layers (list l)))
      )
    )
  )
  (setq layers
    (vl-sort layers
      '(lambda (a b) (< (strcase a) (strcase b)))))
  (setq *LI:UNASSIGNED-LAST-COUNT* (length layers))
  (setq *LI:UNASSIGNED-LAST-XREF*  xrefCnt)
  (if (null layers)
    (progn
      (princ (strcat "\n[Инфо] Список «" *LI:UNASSIGNED-FILTER-NAME*
                     "» пуст — фильтр не создаётся."))
      (setq *LI:ARF-NAME-OVERRIDE* nil)
      nil
    )
    (if (LI:ARF-Create layers)
      (progn
        (princ (strcat "\n[OK] Фильтр '" *LI:UNASSIGNED-FILTER-NAME*
                       "' создан: " (itoa (length layers)) " слоёв."))
        (princ (strcat "\n\n=== Фильтр "
                       *LI:UNASSIGNED-FILTER-NAME* " ==="))
        (princ (strcat "\n  " (LI:PadRight "Слой" 45) "| Статус"))
        (princ (strcat "\n  " (LI:RepeatChar "-" 45) "+"
                       (LI:RepeatChar "-" 10)))
        (foreach name layers
          (princ (strcat "\n  " (LI:PadRight name 45) "| [OK]"))
        )
        (princ (strcat "\n\n  Слоёв в " *LI:UNASSIGNED-FILTER-NAME*
                       ": " (itoa (length layers))))
        (princ (strcat "\n  Слоёв в старых фильтрах: "
                       (itoa (length legacyLayers))))
        (princ (strcat "\n  Пропущено XREF: " (itoa xrefCnt)))
        (princ (strcat "\n  Пропущено системных: " (itoa sysCnt)))
        (princ "\n\n")
        (setq *LI:ARF-NAME-OVERRIDE* nil)
        (length layers)
      )
      (progn
        (princ (strcat "\n[Ошибка] Не удалось создать фильтр «"
                       *LI:UNASSIGNED-FILTER-NAME* "»."))
        (setq *LI:ARF-NAME-OVERRIDE* nil)
        nil
      )
    )
  )
)

;;; Команда: шаблон -> определения фильтров -> список АР -> создание.
(defun LI:RunAR (/ fname xml parsedDefs)
  (princ "\n\n=== Фильтр АР ===")
  (setq fname (LI:FindTemplateFile))
  (if fname
    (progn
      (princ (strcat "\nШаблон: " fname))
      (setq xml (LI:ReadFile fname))
      (if xml
        (setq parsedDefs (LI:ParseFilterSheet xml))
      )
      (if (null parsedDefs)
        (progn
          (princ "\n[Предупр.] Фильтры шаблона не разобраны — беру фильтры чертежа.")
          (setq parsedDefs (LI:ARF-DrawingDefs))
        )
      )
    )
    (progn
      (princ "\n[Инфо] Шаблон не найден — беру фильтры чертежа.")
      (setq parsedDefs (LI:ARF-DrawingDefs))
    )
  )
  (LI:ARF-RunFromDefs parsedDefs)
  (princ)
)

(defun C:МОИСЛОИФИЛЬТРАР () (LI:RunAR) (princ))

;;; ============================================================
;;; Переключение видимости фасадных фильтров (МОИФАСАД*)
;;; Обход дерева групповых фильтров чертежа (ACLYDICTIONARY)
;;; на любом уровне вложенности (новый и старый шаблон).
;;; ============================================================

(defun LI:FilterObjEname (ref / e)
  (cond
    ((= (type ref) 'ENAME) ref)
    ((= (type ref) 'STR) (if (setq e (handent ref)) e nil))
    (t nil)
  )
)

(defun LI:FilterObjData (ref / e d)
  (setq e (LI:FilterObjEname ref))
  (if e
    (progn
      (setq d (vl-catch-all-apply 'entget (list e)))
      (if (LI:IsError d) nil d)
    )
    nil
  )
)

(defun LI:FilterExtDict (data / tail ref)
  (setq ref nil)
  (if (setq tail (member '(102 . "{ACAD_XDICTIONARY") data))
    (setq ref (cdr (assoc 360 tail)))
  )
  (if (null ref)
    (setq ref (cdr (assoc 360 data)))
  )
  (LI:FilterObjEname ref)
)

(defun LI:FilterDictEntry (dict refName / e d)
  (setq d nil)
  (if (setq e (LI:FilterObjEname dict))
    (progn
      (setq d (vl-catch-all-apply 'dictsearch (list e refName)))
      (if (LI:IsError d) (setq d nil))
    )
  )
  d
)

(defun LI:FilterContainer (data / ext d)
  (setq d nil)
  (if (setq ext (LI:FilterExtDict data))
    (progn
      (setq d (LI:FilterDictEntry ext "ACLYDICTIONARY"))
      (if (null d)
        (setq d (LI:FilterObjData ext))
      )
    )
  )
  d
)

(defun LI:FilterDataLayers (data / out pair code objdef lname)
  (setq out nil)
  (foreach pair data
    (setq code (car pair))
    (if (or (= code 330) (= code 340) (= code 350) (= code 360))
      (progn
        (setq objdef (LI:FilterObjData (cdr pair)))
        (if (and objdef (= (strcase (LI:ForceString (cdr (assoc 0 objdef)))) "LAYER"))
          (progn
            (setq lname (cdr (assoc 2 objdef)))
            (if (and (LI:IsString lname) (/= lname ""))
              (setq out (LI:SafeAddUnique out lname))
            )
          )
        )
      )
    )
  )
  out
)

(defun LI:FilterRootDictData ( / layer0 tableExt ext d)
  (setq d nil)
  (if (setq layer0 (tblobjname "LAYER" "0"))
    (progn
      (setq tableExt (LI:FilterObjEname (cdr (assoc 330 (entget layer0)))))
      (if tableExt
        (progn
          (setq ext (LI:FilterExtDict (entget tableExt)))
          (if ext
            (progn
              (setq d (LI:FilterDictEntry ext "ACLYDICTIONARY"))
              (if (null d)
                (setq d (LI:FilterObjData ext))
              )
            )
          )
        )
      )
    )
  )
  d
)

;;; Рекурсивный обход словаря групповых фильтров чертежа (любая глубина).
;;; Возвращает список записей: (имя_фильтра список_всех_слоёв_с_дочерними)
(defun LI:FilterWalkTree (data visited / out pair ref objdef lname
                          childEntries directLayers allLayers cEntry cLayer)
  (setq out nil)
  (if (listp data)
    (foreach pair data
      (if (= (car pair) 350)
        (progn
          (setq ref (cdr pair))
          (if (not (member ref visited))
            (progn
              (setq objdef (LI:FilterObjData ref))
              (cond
                ((and objdef
                      (setq lname (cdr (assoc 300 objdef)))
                      (LI:IsString lname))
                  (setq lname (LI:SafeTrim lname))
                  (setq childEntries
                    (LI:FilterWalkTree
                      (LI:FilterContainer objdef)
                      (cons ref visited)))
                  (setq directLayers (LI:FilterDataLayers objdef))
                  (setq allLayers directLayers)
                  (foreach cEntry childEntries
                    (foreach cLayer (cadr cEntry)
                      (setq allLayers (LI:SafeAddUnique allLayers cLayer))
                    )
                  )
                  (setq out
                    (append out
                            (list (list lname allLayers))
                            childEntries))
                )
                ((and objdef
                      (= (strcase (LI:ForceString (cdr (assoc 0 objdef))))
                         "DICTIONARY"))
                  (setq childEntries
                    (LI:FilterWalkTree objdef (cons ref visited)))
                  (setq out (append out childEntries))
                )
              )
            )
          )
        )
      )
    )
  )
  out
)

;;; Имена непосредственных дочерних групповых фильтров в контейнере.
(defun LI:FilterChildNames (data visited / out pair ref objdef lname)
  (setq out nil)
  (if (listp data)
    (foreach pair data
      (if (= (car pair) 350)
        (progn
          (setq ref (cdr pair))
          (if (not (member ref visited))
            (progn
              (setq objdef (LI:FilterObjData ref))
              (cond
                ((and objdef
                      (setq lname (cdr (assoc 300 objdef)))
                      (LI:IsString lname))
                  (setq out (LI:SafeAddUnique out (LI:SafeTrim lname)))
                )
                ((and objdef
                      (= (strcase (LI:ForceString (cdr (assoc 0 objdef))))
                         "DICTIONARY"))
                  (foreach lname (LI:FilterChildNames objdef (cons ref visited))
                    (setq out (LI:SafeAddUnique out lname))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  out
)

;;; Полное дерево фильтров: (имя прямые_слои дочерние_имена все_слои).
;;; Родитель идёт перед потомками, порядок — как в ACLYDICTIONARY.
(defun LI:FilterTreeFull (data visited / out pair ref objdef lname cont
                            direct childNames sub all cEntry cLayer)
  (setq out nil)
  (if (listp data)
    (foreach pair data
      (if (= (car pair) 350)
        (progn
          (setq ref (cdr pair))
          (if (not (member ref visited))
            (progn
              (setq objdef (LI:FilterObjData ref))
              (cond
                ((and objdef
                      (setq lname (cdr (assoc 300 objdef)))
                      (LI:IsString lname))
                  (setq lname (LI:SafeTrim lname))
                  (setq cont (LI:FilterContainer objdef))
                  (setq direct (LI:FilterDataLayers objdef))
                  (setq childNames (LI:FilterChildNames cont (cons ref visited)))
                  (setq sub (LI:FilterTreeFull cont (cons ref visited)))
                  (setq all direct)
                  (foreach cEntry sub
                    (foreach cLayer (nth 3 cEntry)
                      (setq all (LI:SafeAddUnique all cLayer))
                    )
                  )
                  (setq out
                    (append out
                            (list (list lname direct childNames all))
                            sub))
                )
                ((and objdef
                      (= (strcase (LI:ForceString (cdr (assoc 0 objdef))))
                         "DICTIONARY"))
                  (foreach cEntry (LI:FilterTreeFull objdef (cons ref visited))
                    (setq out (append out (list cEntry)))
                  )
                )
              )
            )
          )
        )
      )
    )
  )
  out
)

;;; Получить слои фильтра по списку возможных имён (новый + старый шаблон).
;;; Если фильтр в чертеже не найден — возвращает defaultLayers.
(defun LI:ResolveFacadeFilterLayers (candidateNames defaultLayers tree
                                     / found cName entry lyr)
  (setq found nil)
  (foreach cName candidateNames
    (foreach entry tree
      (if (= (LI:SafeStrCase (car entry)) (LI:SafeStrCase cName))
        (foreach lyr (cadr entry)
          (setq found (LI:SafeAddUnique found lyr))
        )
      )
    )
  )
  (if found found defaultLayers)
)

;;; Установить видимость одного слоя (visible = T -> вкл, nil -> выкл)
;;; через знак DXF-кода 62 (работает даже для текущего слоя CLAYER без запросов).
;;; Возвращает: T (изменён/установлен) или nil (слой не найден в чертеже).
(defun LI:SetLayerVisible (name visible / ename ent c62 aci newColor res)
  (setq name (LI:SafeTrim name))
  (if (= name "")
    nil
    (progn
      (setq ename (tblobjname "LAYER" name))
      (if (not ename)
        nil
        (progn
          (setq ent (entget ename))
          (setq c62 (cdr (assoc 62 ent)))
          (if (or (null c62) (= c62 0))
            (setq aci 7)
            (setq aci (abs c62))
          )
          (setq newColor (if visible aci (- aci)))
          (setq ent (LI:SetDxf ent 62 newColor))
          (setq res (vl-catch-all-apply 'entmod (list ent)))
          (not (LI:IsError res))
        )
      )
    )
  )
)

;;; Является ли слой собственным слоем подсистемы (для отключения в МОИФАСАДФАС)
(defun LI:IsSubsysLayer (name / up)
  (setq up (LI:SafeStrCase (LI:SafeTrim name)))
  (or (LI:NameInList *LI:FACADE-SUBSYS-LAYERS* name)
      (wcmatch up "*ПОДСИСТЕМ*"))
)

;;; Универсальное переключение фасадного режима:
;;;   1) выключает все слои из offList
;;;   2) включает все слои из onList
;;;   3) выключает все слои из postOffList (если передан)
(defun LI:ApplyFacadeMode (modeTitle offList onList postOffList
                           / lyr finalOn finalOff onOk offOk missCnt)
  (setq finalOn nil)
  (setq finalOff nil)
  (setq onOk 0)
  (setq offOk 0)
  (setq missCnt 0)

  ;; Шаг 1: сначала выключаем
  (foreach lyr offList
    (LI:SetLayerVisible lyr nil)
  )

  ;; Шаг 2: затем включаем (пересекающиеся слои остаются включёнными)
  (foreach lyr onList
    (LI:SetLayerVisible lyr T)
  )

  ;; Шаг 3: дополнительное отключение (например, слои подсистем в МОИФАСАДФАС)
  (foreach lyr postOffList
    (LI:SetLayerVisible lyr nil)
  )

  ;; Формируем итоговые списки для отчёта
  (foreach lyr onList
    (if (not (LI:NameInList postOffList lyr))
      (setq finalOn (LI:SafeAddUnique finalOn lyr))
    )
  )
  (foreach lyr offList
    (if (not (LI:NameInList finalOn lyr))
      (setq finalOff (LI:SafeAddUnique finalOff lyr))
    )
  )
  (foreach lyr postOffList
    (if (not (LI:NameInList finalOn lyr))
      (setq finalOff (LI:SafeAddUnique finalOff lyr))
    )
  )

  (princ (strcat "\n\n=== " modeTitle " ==="))
  (princ (strcat "\n  "
                 (LI:PadRight "Статус" 10) "| "
                 (LI:PadRight "Имя слоя" 42) "| "
                 "В чертеже"))
  (princ (strcat "\n  "
                 (LI:RepeatChar "-" 10) "+"
                 (LI:RepeatChar "-" 43) "+"
                 (LI:RepeatChar "-" 12)))

  (foreach lyr finalOn
    (if (tblobjname "LAYER" lyr)
      (progn
        (setq onOk (1+ onOk))
        (princ (strcat "\n  "
                       (LI:PadRight "[ВКЛ]" 10) "| "
                       (LI:PadRight lyr 42) "| да"))
      )
      (progn
        (setq missCnt (1+ missCnt))
        (princ (strcat "\n  "
                       (LI:PadRight "[ВКЛ]" 10) "| "
                       (LI:PadRight lyr 42) "| нет слоя"))
      )
    )
  )

  (foreach lyr finalOff
    (if (tblobjname "LAYER" lyr)
      (progn
        (setq offOk (1+ offOk))
        (princ (strcat "\n  "
                       (LI:PadRight "[ВЫКЛ]" 10) "| "
                       (LI:PadRight lyr 42) "| да"))
      )
      (progn
        (setq missCnt (1+ missCnt))
        (princ (strcat "\n  "
                       (LI:PadRight "[ВЫКЛ]" 10) "| "
                       (LI:PadRight lyr 42) "| нет слоя"))
      )
    )
  )

  (vl-catch-all-apply 'vl-cmdf (list "._REGEN"))
  (princ (strcat "\n\nИтог: включено слоёв: " (itoa onOk)
                 ", выключено слоёв: " (itoa offOk)
                 (if (> missCnt 0)
                   (strcat " (отсутствует в чертеже: " (itoa missCnt) ")")
                   "")))
  (princ)
)

;;; Выполнение одного из 4 режимов: "OBL", "POD", "UTEP", "FAS"
(defun LI:RunFacadePreset (mode / tree oblLayers podLayers utepLayers
                           offList onList postOffList lyr)
  (setq tree (LI:FilterWalkTree (LI:FilterRootDictData) nil))
  (setq oblLayers
    (LI:ResolveFacadeFilterLayers
      *LI:FACADE-OBL-FILTERS*
      *LI:FACADE-OBL-DEFAULT-LAYERS*
      tree))
  (setq podLayers
    (LI:ResolveFacadeFilterLayers
      *LI:FACADE-POD-FILTERS*
      *LI:FACADE-POD-DEFAULT-LAYERS*
      tree))
  (setq utepLayers
    (LI:ResolveFacadeFilterLayers
      *LI:FACADE-UTEP-FILTERS*
      *LI:FACADE-UTEP-DEFAULT-LAYERS*
      tree))

  (setq offList nil)
  (setq onList nil)
  (setq postOffList nil)

  (cond
    ;; МОИФАСАДОБЛ: выкл (Подсистема + Утепление + Железо) -> вкл (Облицовка)
    ((= mode "OBL")
      (foreach lyr podLayers  (setq offList (LI:SafeAddUnique offList lyr)))
      (foreach lyr utepLayers (setq offList (LI:SafeAddUnique offList lyr)))
      (setq offList (LI:SafeAddUnique offList *LI:FACADE-IRON-LAYER*))
      (foreach lyr oblLayers  (setq onList  (LI:SafeAddUnique onList  lyr)))
      (LI:ApplyFacadeMode
        "МОИФАСАДОБЛ: вкл Фасад - Облицовка"
        offList onList nil)
    )

    ;; МОИФАСАДПОД: выкл (Облицовка + Утепление + Железо) -> вкл (Подсистема)
    ((= mode "POD")
      (foreach lyr oblLayers  (setq offList (LI:SafeAddUnique offList lyr)))
      (foreach lyr utepLayers (setq offList (LI:SafeAddUnique offList lyr)))
      (setq offList (LI:SafeAddUnique offList *LI:FACADE-IRON-LAYER*))
      (foreach lyr podLayers  (setq onList  (LI:SafeAddUnique onList  lyr)))
      (LI:ApplyFacadeMode
        "МОИФАСАДПОД: вкл Фасад - Подсистема"
        offList onList nil)
    )

    ;; МОИФАСАДУТЕП: выкл (Подсистема + Облицовка + Железо) -> вкл (Утепление)
    ((= mode "UTEP")
      (foreach lyr podLayers  (setq offList (LI:SafeAddUnique offList lyr)))
      (foreach lyr oblLayers  (setq offList (LI:SafeAddUnique offList lyr)))
      (setq offList (LI:SafeAddUnique offList *LI:FACADE-IRON-LAYER*))
      (foreach lyr utepLayers (setq onList  (LI:SafeAddUnique onList  lyr)))
      (LI:ApplyFacadeMode
        "МОИФАСАДУТЕП: вкл Фасад - Утепление"
        offList onList nil)
    )

    ;; МОИФАСАДФАС: выкл (Облицовка + Утепление) -> вкл (Подсистема + Железо) -> выкл (слои подсистем)
    ((= mode "FAS")
      (foreach lyr oblLayers  (setq offList (LI:SafeAddUnique offList lyr)))
      (foreach lyr utepLayers (setq offList (LI:SafeAddUnique offList lyr)))
      (foreach lyr podLayers  (setq onList  (LI:SafeAddUnique onList  lyr)))
      (setq onList (LI:SafeAddUnique onList *LI:FACADE-IRON-LAYER*))
      (foreach lyr *LI:FACADE-SUBSYS-LAYERS*
        (setq postOffList (LI:SafeAddUnique postOffList lyr))
      )
      (foreach lyr podLayers
        (if (LI:IsSubsysLayer lyr)
          (setq postOffList (LI:SafeAddUnique postOffList lyr))
        )
      )
      (LI:ApplyFacadeMode
        "МОИФАСАДФАС: вкл Железо + контуры подсистемы"
        offList onList postOffList)
    )
  )
  (princ)
)

(defun C:МОИФАСАДОБЛ  () (LI:RunFacadePreset "OBL"))
(defun C:МОИФАСАДПОД  () (LI:RunFacadePreset "POD"))
(defun C:МОИФАСАДУТЕП () (LI:RunFacadePreset "UTEP"))
(defun C:МОИФАСАДФАС  () (LI:RunFacadePreset "FAS"))

;;; ============================================================
;;; Сообщение при загрузке
;;; ============================================================

(princ "\nЗагружено: MyLayer.lsp (ред. 2) — слои, фильтры и переменные слоёв.")
(princ "\nОсновные команды:")
(princ "\n  +---------------------------------------------------------------+")
(princ "\n  | КОМАНДА                  | ЧТО ДЕЛАЕТ                         |")
(princ "\n  +---------------------------------------------------------------+")
(princ "\n  | МОИСЛОИСОЗДАТЬ           | слои шаблона + переменные          |")
(princ "\n  | МОИСЛОИПОУМОЛЧАНИЮ       | только применить переменные        |")
(princ "\n  | МОИСЛОИТЕКУЩИЕ           | текущие значения переменных        |")
(princ "\n  | МОИСЛОИВЫГРУЗКА          | выгрузка в Слои и фильтры.xml      |")
(princ "\n  | МОИСЛОИЗАГРУЗИТЬ         | слои + фильтры + переменные        |")
(princ "\n  | МОИСЛОИФИЛЬТРЫ           | только фильтры (без слоёв)         |")
(princ "\n  | МОИСЛОИФИЛЬТРЫОЧИСТИТЬ   | удалить фильтры из XML             |")
(princ "\n  | МОИФАСАДОБЛ              | вкл: Облицовка (ост. фасад выкл)   |")
(princ "\n  | МОИФАСАДПОД              | вкл: Подсистема (ост. фасад выкл)  |")
(princ "\n  | МОИФАСАДУТЕП             | вкл: Утепление (ост. фасад выкл)   |")
(princ "\n  | МОИФАСАДФАС              | вкл: Железо + контуры подсистемы   |")
(princ "\n  | МОИСЛОИФИЛЬТРАР          | фильтр АР: всё, что не в фильтрах  |")
(princ "\n  | МОИСЛОИПРОВЕРКАФАЙЛА     | проверка скобок в LISP-файле       |")
(princ "\n  +---------------------------------------------------------------+")
(princ)