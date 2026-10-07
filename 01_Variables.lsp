;;; ============================================================
;;; 01_Variables.lsp
;;;
;;; Модуль установки переменных, отвечающих за слои
;;; создаваемых объектов.
;;;
;;; Команды:
;;; МОИСЛОИСОЗДАТЬ         - создать слои шаблона + применить переменные
;;; МОИСЛОИПОУМОЛЧАНИЮ     - применить переменные
;;; МОИСЛОИТЕКУЩИЕ         - показать текущие и требуемые значения
;;; ============================================================

(vl-load-com)

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

(princ "\n=============================================")
(princ "\nЗагружено: 01_Variables.lsp")
(princ "\nКоманды:")
(princ "\n  МОИСЛОИСОЗДАТЬ       - создать слои шаблона + переменные")
(princ "\n  МОИСЛОИПОУМОЛЧАНИЮ   - только переменные")
(princ "\n  МОИСЛОИТЕКУЩИЕ       - показать текущие значения")
(princ "\n=============================================")
(princ)