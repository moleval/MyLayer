;;; ============================================================
;;; 01_Variables.lsp
;;;
;;; Модуль установки переменных, отвечающих за слои
;;; создаваемых объектов.
;;;
;;; Команды:
;;; МОИСЛОИСОЗДАТЬ      - создать слои и применить переменные
;;; МОИСЛОИПРИМЕНИТЬ    - применить переменные
;;; МОИСЛОИТЕКУЩИЕ    - показать текущие и требуемые значения
;;;
;;; Английские аналоги:
;;; SETMYVARS
;;; SETMYVARSNO
;;; SHOWMYVARS
;;; ============================================================

(vl-load-com)

;;; ========================= НАСТРОЙКИ =========================

;; Показывать пропущенные неподдерживаемые переменные.

(setq *LV:SHOW-SKIPPED* T)

;; Показывать таблицу созданных слоёв.

(setq *LV:SHOW-CREATED-LAYERS* T)

;; Список переменных:
;; 0 - имя системной переменной
;; 1 - русская подсказка
;; 2 - требуемое значение / имя слоя

(setq *LV:VARS*
  '(
    ("TEXTLAYER"     "Текстовый слой"                         "Текст")
    ("DIMLAYER"      "Размерный слой"                         "Размеры")
    ("HPLAYER"       "Слой штриховки"                         "Штриховка")
    ("MLEADERLAYER"  "Слой мультивыносок"                     "Обозначения")
    ("CENTERLAYER"   "Слой осевых и центровых линий"         "Штрихпунктирные и осевые центры")
    ("TABLELAYER"    "Слой таблиц"                            "Размеры")
    ("REVCLOUDLAYER" "Слой ревизионных облаков"              "Тонкая")
    ("VIEWPORTLAYER" "Слой видовых экранов"                  "Невидимые")
    ("XREFLAYER"     "Слой внешних ссылок"                   "0")
   )
)

;;; ============================================================
;;; Базовые функции
;;; ============================================================

(defun LV:IsString (x)
  (eq (type x) 'STR)
)

(defun LV:IsError (x)
  (if x
    (vl-catch-all-error-p x)
    nil
  )
)

(defun LV:CellStr (x)
  (cond
    ((null x)
      ""
    )

    ((LV:IsString x)
      x
    )

    (t
      (vl-prin1-to-string x)
    )
  )
)

(defun LV:Repeat (s n / out)
  (if (not (numberp n))
    (setq n 0)
  )

  (if (< n 0)
    (setq n 0)
  )

  (setq out "")

  (repeat (fix n)
    (setq out (strcat out s))
  )

  out
)

(defun LV:Pad (s w / l)
  (setq s (LV:CellStr s))

  (if (not (numberp w))
    (setq w 0)
  )

  (setq l (strlen s))

  (if (>= l w)
    (strcat s " ")
    (strcat s (LV:Repeat " " (- w l)) " ")
  )
)

(defun LV:ListSetMax (lst idx val / i out len)
  (setq i 0)
  (setq out nil)
  (setq len (length lst))

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
      (if (and
            (LV:IsString x)
            (= (strcase x) (strcase name))
          )
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
        (setq widths
          (append
            widths
            (list (strlen (LV:CellStr h)))
          )
        )
      )

      (foreach row rows
        (setq i 0)

        (foreach cell row
          (setq widths
            (LV:ListSetMax
              widths
              i
              (strlen (LV:CellStr cell))
            )
          )
          (setq i (1+ i))
        )
      )

      (if (and title (/= title ""))
        (princ (strcat "\n" title))
      )

      ;; Заголовок таблицы

      (setq line "")
      (setq i 0)

      (foreach h headers
        (setq line
          (strcat
            line
            (LV:Pad h (nth i widths))
          )
        )
        (setq i (1+ i))
      )

      (princ (strcat "\n" line))

      ;; Разделитель

      (setq sep "")

      (foreach w widths
        (setq sep
          (strcat
            sep
            (LV:Pad (LV:Repeat "-" w) w)
          )
        )
      )

      (princ (strcat "\n" sep))

      ;; Строки таблицы

      (foreach row rows
        (setq line "")
        (setq i 0)

        (foreach cell row
          (setq line
            (strcat
              line
              (LV:Pad cell (nth i widths))
            )
          )
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

  (not
    (or
      (LV:IsError raw)
      (null raw)
    )
  )
)

(defun LV:GetVarSafe (var / raw)
  (setq raw (LV:GetVarRaw var))

  (cond
    ((LV:IsError raw)
      "<ошибка>"
    )
    ((null raw)
      "<нет / не поддерживается>"
    )
    ((LV:IsString raw)
      raw
    )
    (t
      (vl-prin1-to-string raw)
    )
  )
)

(defun LV:SetVar (var val / res)
  (setq res
    (vl-catch-all-apply
      'setvar
      (list var val)
    )
  )

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
    (or
      (= (strcase name) "0")
      (tblsearch "LAYER" name)
    )
  )
)

;; Возвращает:
;; "exists"  - слой уже существует
;; "created" - слой создан
;; "failed"  - не удалось создать слой

(defun LV:EnsureLayer (doc name / layers res)
  (if (not (LV:IsString name))
    (setq name (vl-prin1-to-string name))
  )

  (if (or (not name) (= name ""))
    "failed"
    (if (LV:LayerExists name)
      "exists"
      (progn
        (setq layers
          (vl-catch-all-apply
            'vla-get-Layers
            (list doc)
          )
        )

        (if (LV:IsError layers)
          "failed"
          (progn
            (setq res
              (vl-catch-all-apply
                'vla-add
                (list layers name)
              )
            )

            (if (LV:IsError res)
              "failed"
              "created"
            )
          )
        )
      )
    )
  )
)

;;; ============================================================
;;; Основная логика установки переменных
;;; ============================================================

(defun LV:SetAll
       (
        createAll
        autoCreateOnFail
        /
        doc
        item
        var
        hint
        val
        supported
        supportedCount
        setCount
        skipCount
        varRows
        createdLayers
        failedLayers
        layerStatus
        ok
        layerExists
        createdRows
        failedRows
       )

  (setq doc
    (vla-get-ActiveDocument
      (vlax-get-acad-object)
    )
  )

  (setq supportedCount 0)
  (setq setCount 0)
  (setq skipCount 0)

  (setq varRows nil)
  (setq createdLayers nil)
  (setq failedLayers nil)

  (foreach item *LV:VARS*
    (setq var  (nth 0 item))
    (setq hint (nth 1 item))
    (setq val  (nth 2 item))

    (setq supported (LV:VarSupported var))

    ;; В режиме МОИСЛОИСОЗДАТЬ создаём все слои из списка,
    ;; даже если переменная не поддерживается.

    (if createAll
      (progn
        (setq layerStatus (LV:EnsureLayer doc val))

        (cond
          ((= layerStatus "created")
            (setq createdLayers
              (LV:AddUnique createdLayers val)
            )
          )
          ((= layerStatus "failed")
            (setq failedLayers
              (LV:AddUnique failedLayers val)
            )
          )
        )
      )
    )

    (if supported
      (progn
        (setq supportedCount (1+ supportedCount))

        (setq ok (LV:SetVar var val))

        ;; Если переменная не установилась и слоя нет,
        ;; пробуем создать слой и повторить установку.

        (if (not ok)
          (progn
            (setq layerExists (LV:LayerExists val))

            (if (and
                  autoCreateOnFail
                  (not createAll)
                  (not layerExists)
                )
              (progn
                (setq layerStatus (LV:EnsureLayer doc val))

                (cond
                  ((= layerStatus "created")
                    (setq createdLayers
                      (LV:AddUnique createdLayers val)
                    )
                  )
                  ((= layerStatus "failed")
                    (setq failedLayers
                      (LV:AddUnique failedLayers val)
                    )
                  )
                )

                (if (not (= layerStatus "failed"))
                  (setq ok (LV:SetVar var val))
                )
              )
            )
          )
        )

        (if ok
          (progn
            (setq setCount (1+ setCount))

            (setq varRows
              (append
                varRows
                (list
                  (list "Установлено" var val hint)
                )
              )
            )
          )
          (setq varRows
            (append
              varRows
              (list
                (list "Ошибка" var val hint)
              )
            )
          )
        )
      )
      (progn
        (setq skipCount (1+ skipCount))

        (if *LV:SHOW-SKIPPED*
          (setq varRows
            (append
              varRows
              (list
                (list "Пропущено" var val hint)
              )
            )
          )
        )
      )
    )
  )

  ;; Таблица результатов установки переменных

  (if varRows
    (LV:PrintTable
      "Установка переменных"
      '("Статус" "Переменная" "Значение" "Подсказка")
      varRows
    )
  )

  ;; Таблица созданных слоёв

  (if (and *LV:SHOW-CREATED-LAYERS* createdLayers)
    (progn
      (setq createdRows nil)

      (foreach n createdLayers
        (setq createdRows
          (append
            createdRows
            (list (list n))
          )
        )
      )

      (LV:PrintTable
        "Созданные слои"
        '("Имя слоя")
        createdRows
      )
    )
  )

  ;; Таблица слоёв, которые не удалось создать

  (if failedLayers
    (progn
      (setq failedRows nil)

      (foreach n failedLayers
        (setq failedRows
          (append
            failedRows
            (list (list n))
          )
        )
      )

      (LV:PrintTable
        "Не удалось создать слои"
        '("Имя слоя")
        failedRows
      )
    )
  )

  (princ
    (strcat
      "\nУстановлено переменных: "
      (itoa setCount)
      " из "
      (itoa supportedCount)
      " поддерживаемых"
    )
  )

  (if (and
        (> skipCount 0)
        (not *LV:SHOW-SKIPPED*)
      )
    (princ
      (strcat
        "\nПропущено неподдерживаемых переменных: "
        (itoa skipCount)
      )
    )
  )
)

;;; ============================================================
;;; Команды
;;; ============================================================

(defun C:SETMYVARS ()
  (LV:SetAll T T)
  (princ)
)

(defun C:SETMYVARSNO ()
  (LV:SetAll nil T)
  (princ)
)

(defun C:SHOWMYVARS ( / rows item var hint need)
  (setq rows nil)

  (foreach item *LV:VARS*
    (setq var  (nth 0 item))
    (setq hint (nth 1 item))
    (setq need (nth 2 item))

    (setq rows
      (append
        rows
        (list
          (list
            var
            (LV:GetVarSafe var)
            need
            hint
          )
        )
      )
    )
  )

  (LV:PrintTable
    "Текущие значения переменных"
    '("Переменная" "Текущее" "Нужно" "Подсказка")
    rows
  )

  (princ)
)

;;; ============================================================
;;; Русские команды
;;; ============================================================

(defun C:МОИСЛОИСОЗДАТЬ ()
  (C:SETMYVARS)
)

(defun C:МОИСЛОИПРИМЕНИТЬ ()
  (C:SETMYVARSNO)
)

(defun C:МОИСЛОИТЕКУЩИЕ ()
  (C:SHOWMYVARS)
)

(princ "\n=============================================")
(princ "\nЗагружено: 01_Variables.lsp")
(princ "\nКоманды:")
(princ "\n  МОИСЛОИСОЗДАТЬ")
(princ "\n  МОИСЛОИПРИМЕНИТЬ")
(princ "\n  МОИСЛОИТЕКУЩИЕ")
(princ "\n=============================================")
(princ)