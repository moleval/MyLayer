;;; ============================================================
;;; MyLayer.lsp — МОНОЛИТ модулей слоёв AutoCAD
;;; Маркер редакции для RELOAD: (ред. 1)
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

(defun LX:OutFile (/ p)
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
  (strcat p "Слои и фильтры.xml")
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
       (fh doc / layers layer name ent flags color
              desc linetype lw plotflag transparency
              plotStyle colorName red green blue colorMethod
              row count allRows sortedRows rec
              layerObj rgb rVal gVal bVal tcAny)
  (write-line "<Worksheet ss:Name=\"Слои\"><Table>" fh)
  (write-line
    (XML:Row
      '("Имя слоя" "Описание" "Цвет ACI" "Цвет имя" "Цвет метод"
        "R" "G" "B" "Тип линии" "Вес линии код" "Вес линии текст"
        "Прозрачность" "Стиль печати" "Включен" "Заморожен"
        "Заморожен в новых ВЭ" "Заблокирован" "Печатается"
        "Зависит от XREF" "Флаги 70"))
    fh)

  (setq allRows nil)
  (setq layers (vla-get-Layers doc))

  ;; ---------- Сбор данных ----------
  (vlax-for layer layers
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

    (setq allRows (append allRows (list (cons name row))))
  )

  ;; ---------- Сортировка по имени без учёта регистра ----------
  (setq sortedRows
    (vl-sort allRows
      '(lambda (a b)
         (< (strcase (car a)) (strcase (car b))))))

  ;; ---------- Печать в командную строку ----------
  (princ "\n\n=== Экспорт слоёв ===")
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

(defun LX:WriteFiltersSheet (fh / items item key count)
  (write-line "<Worksheet ss:Name=\"Фильтры\"><Table>" fh)
  (write-line (XML:Row '("Имя фильтра" "Список слоев")) fh)
  (princ "\n\n=== Экспорт фильтров ===")
  (princ (strcat "\n  "
                 (LX:PadRight "Имя фильтра" 30) "| Список слоёв"))
  (princ (strcat "\n  "
                 (LX:RepeatChar "-" 30) "+"
                 (LX:RepeatChar "-" 40)))
  (setq items (LX:GetLayerFilters))
  (setq count 0)
  (if items
    (foreach item items
      (setq key (car item))
      (write-line (XML:Row (list key "")) fh)
      (princ (strcat "\n  "
                     (LX:PadRight key 30) "| (заполнить в Excel)"))
      (setq count (1+ count)))
    (princ "\n  (фильтры не найдены)"))
  (write-line "</Table></Worksheet>" fh)
  (princ "\n")
  count
)

;;; ============================================================
;;; Основная команда
;;; ============================================================

(defun LX:Export (/ doc fname fh tmpdir layerCount filterCount)
  (vl-load-com)
  (princ "\n")
  (princ "\n=== Экспорт слоёв и фильтров в XML ===")
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq fname (LX:OutFile))
  (princ (strcat "\nЧертёж: " (getvar "DWGNAME")))
  (princ (strcat "\nФайл:   " fname))
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
      (write-line "   <Font ss:FontName=\"Arial\" ss:Size=\"10\"/>" fh)
      (write-line "   <Interior/>" fh)
      (write-line "   <NumberFormat/>" fh)
      (write-line "   <Protection/>" fh)
      (write-line "  </Style>" fh)
      (write-line " </Styles>" fh)
      (setq layerCount  (LX:WriteLayersSheet  fh doc))
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

;; Старые фильтры слоёв (удаляются при импорте и очистке фильтров)
(setq *LI:OLD-FILTERS*
  '(
    "Витражи"
    "Фасады"
    "Фонарь 3D"
  )
)

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
  (if (and name (/= name ""))
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
;;; Импорт фильтров
;;; ============================================================

(defun LI:ImportFilters
       (doc xml / sheet rows headerIndex headers dataRows
        map row vals name layersList filterDefs uniqueDefs
        def parsedDefs p raw parsed direct children
        directString fullLayers orderedDefs reversedDefs delNames
        createdNames parentName delPass ok parentLabel layerCount
        existing tmp mergedCount)
  (princ "\n\n")
  (princ "\nЧтение фильтров слоёв...")
  (setq filterDefs nil)
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
          (setq orderedDefs (LI:BuildFilterOrder parsedDefs))
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
          (setq delPass 1)
          (while (<= delPass 2)
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
          (princ "\nИмпорт фильтров завершён.")
        )
      )
    )
  )
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

(defun LI:LocalTemplateFile (/ dir base candidates f result)
  (setq dir (LI:DrawingDir))
  (if dir
    (progn
      (setq base (getvar "DWGNAME"))
      (if (or (not base) (= base "")) (setq base "Untitled.dwg"))
      (setq base (vl-filename-base base))
      (setq candidates
        (list
          (strcat dir "Слои и фильтры.xml")
          (strcat dir base " Слои.xml")
          (strcat dir base ".xml")
          (strcat dir "Слои.xml")
        )
      )
      (princ (strcat "\n[Поиск] Папка чертежа: " dir))
      (princ (strcat "\n[Поиск] Имя чертежа (без расширения): " base))
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

(defun LI:FindTemplateFile (/ f)
  (setq f (LI:LocalTemplateFile))
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
       (/ fname xml sheet rows vals name names pass)
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
      (princ "\nГотово.")
    )
    (princ "\nСписок фильтров пуст — нечего удалять.")
  )
  (princ)
)

;;; ============================================================
;;; Сообщение при загрузке
;;; ============================================================

(princ "\nЗагружено: MyLayer.lsp (ред. 1) — слои, фильтры и переменные слоёв.")
(princ "\nОсновные команды:")
(princ "\n  +--------------------------------------------------------------+")
(princ "\n  | КОМАНДА                  | ЧТО ДЕЛАЕТ                        |")
(princ "\n  +--------------------------------------------------------------+")
(princ "\n  | МОИСЛОИСОЗДАТЬ           | слои шаблона + переменные         |")
(princ "\n  | МОИСЛОИПОУМОЛЧАНИЮ       | только применить переменные       |")
(princ "\n  | МОИСЛОИТЕКУЩИЕ           | текущие значения переменных       |")
(princ "\n  | МОИСЛОИВЫГРУЗКА          | выгрузка в Слои и фильтры.xml     |")
(princ "\n  | МОИСЛОИЗАГРУЗИТЬ         | слои + фильтры + переменные       |")
(princ "\n  | МОИСЛОИФИЛЬТРЫ           | только фильтры (без слоёв)        |")
(princ "\n  | МОИСЛОИФИЛЬТРЫОЧИСТИТЬ   | удалить фильтры из XML            |")
(princ "\n  +--------------------------------------------------------------+")
(princ)