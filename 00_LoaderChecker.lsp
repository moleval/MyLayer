;;; ============================================================
;;; 00_LoaderChecker.lsp
;;;
;;; Модуль-загрузчик и базовая проверка синтаксиса LISP-файлов:
;;; - проверка скобок;
;;; - проверка незакрытых строк;
;;; - загрузка модулей только после проверки.
;;;
;;; Команды:
;;; ЗАГРУЗЧИК          - проверить и загрузить все модули
;;; ПРОВЕРКАМОДУЛЕЙ    - только проверить все модули
;;; ПРОВЕРКАФАЙЛА      - проверить выбранный вручную LSP-файл
;;;
;;; Английские аналоги:
;;; LISPLOAD
;;; LISPCHECK
;;; LISPCHECKFILE
;;; ============================================================

;;; ========================= НАСТРОЙКИ =========================

;; Папка с модулями.
;; Если файлы лежат в конкретной папке, укажите путь, например:
;; (setq *LC:PATH* "D:/CAD_LSP/")
;;
;; Если оставить "", модули будут искаться через findfile,
;; то есть в текущей папке и в путях поддержки файлов.

(setq *LC:PATH* "")

;; Список загружаемых модулей по порядку.

(setq *LC:MODULES*
  '(
    "01_Variables.lsp"
    "02_LayerExport.lsp"
    "03_LayerImport.lsp"
   )
)

;;; ============================================================
;;; Вспомогательные функции
;;; ============================================================

(defun LC:EnsureSlash (p)
  (if (and p (/= p ""))
    (if (= (substr p (strlen p) 1) "\\")
      p
      (strcat p "\\")
    )
    p
  )
)

(defun LC:FindFile (name / full)
  (if (and *LC:PATH* (/= *LC:PATH* ""))
    (progn
      (setq full
        (strcat
          (LC:EnsureSlash *LC:PATH*)
          name
        )
      )

      (if (findfile full)
        full
        (findfile name)
      )
    )
    (findfile name)
  )
)

(defun LC:ErrorMessage (err / msg)
  (setq msg
    (vl-catch-all-apply
      'vl-catch-all-error-message
      (list err)
    )
  )

  (if (or
        (not msg)
        (vl-catch-all-error-p msg)
      )
    "неизвестная ошибка"
    msg
  )
)

;;; ============================================================
;;; Проверка одного файла
;;; ============================================================

(defun LC:CheckFile
       (
        fname
        /
        fh
        line
        lineNo
        errors
        stack
        inString
        len
        i
        c
       )

  (setq fh (open fname "r"))

  (if (not fh)
    (progn
      (princ
        (strcat
          "\n[Ошибка] Не удалось открыть файл: "
          fname
        )
      )
      nil
    )
    (progn
      (setq lineNo 0)
      (setq errors 0)
      (setq stack nil)

      (while (setq line (read-line fh))
        (setq lineNo (1+ lineNo))

        (setq inString nil)
        (setq len (strlen line))
        (setq i 1)

        (while (<= i len)
          (setq c (substr line i 1))

          (if inString
            (progn
              (if (= c "\\")
                ;; пропускаем экранированный символ,
                ;; например \"
                (setq i (1+ i))
                (if (= c "\"")
                  (setq inString nil)
                )
              )
            )
            (progn
              (cond
                ;; Комментарий до конца строки
                ((= c ";")
                  (setq i len)
                )

                ;; Начало строки
                ((= c "\"")
                  (setq inString T)
                )

                ;; Открывающая скобка
                ((= c "(")
                  (setq stack (cons lineNo stack))
                )

                ;; Закрывающая скобка
                ((= c ")")
                  (if stack
                    (setq stack (cdr stack))
                    (progn
                      (princ
                        (strcat
                          "\n[Ошибка] Лишняя закрывающая скобка в файле:"
                          "\n  Файл: "
                          fname
                          "\n  Строка: "
                          (itoa lineNo)
                        )
                      )

                      (setq errors (1+ errors))
                    )
                  )
                )
              )
            )
          )

          (setq i (1+ i))
        )

        (if inString
          (progn
            (princ
              (strcat
                "\n[Ошибка] Незакрытая строка в файле:"
                "\n  Файл: "
                fname
                "\n  Строка: "
                (itoa lineNo)
              )
            )

            (setq errors (1+ errors))
          )
        )
      )

      (close fh)

      (if stack
        (progn
          (princ
            (strcat
              "\n[Ошибка] Незакрытые открывающие скобки в файле:"
              "\n  Файл: "
              fname
              "\n  Строки:"
            )
          )

          (foreach l (reverse stack)
            (princ (strcat " " (itoa l)))
          )

          (setq errors (1+ errors))
        )
      )

      (if (= errors 0)
        (progn
          (princ
            (strcat
              "\n[OK] "
              fname
            )
          )
          T
        )
        (progn
          (princ
            (strcat
              "\n[FAIL] "
              fname
              "\n  Найдено ошибок: "
              (itoa errors)
            )
          )
          nil
        )
      )
    )
  )
)

;;; ============================================================
;;; Проверка всех модулей
;;; ============================================================

(defun LC:CheckModule (name / full)
  (setq full (LC:FindFile name))

  (if full
    (LC:CheckFile full)
    (progn
      (princ
        (strcat
          "\n[Ошибка] Файл модуля не найден: "
          name
        )
      )
      nil
    )
  )
)

(defun LC:CheckAll (/ allOk moduleOk)
  (setq allOk T)

  (princ "\n=== Проверка модулей ===")

  (foreach m *LC:MODULES*
    (setq moduleOk (LC:CheckModule m))

    (if (not moduleOk)
      (setq allOk nil)
    )
  )

  (if allOk
    (princ "\nВсе модули прошли проверку.")
    (princ "\nПроверка завершена с ошибками.")
  )

  allOk
)

;;; ============================================================
;;; Загрузка всех модулей
;;; ============================================================

(defun LC:LoadAll (/ full res loaded failed moduleOk)
  (setq loaded 0)
  (setq failed 0)

  (princ "\n=== Проверка и загрузка модулей ===")

  (foreach m *LC:MODULES*
    (setq full (LC:FindFile m))

    (if (not full)
      (progn
        (princ
          (strcat
            "\n[Ошибка] Файл модуля не найден: "
            m
          )
        )

        (setq failed (1+ failed))
      )
      (progn
        (setq moduleOk (LC:CheckFile full))

        (if moduleOk
          (progn
            (setq res
              (vl-catch-all-apply
                'load
                (list full)
              )
            )

            (if (vl-catch-all-error-p res)
              (progn
                (princ
                  (strcat
                    "\n[Ошибка] Ошибка загрузки модуля: "
                    full
                    "\n  "
                    (LC:ErrorMessage res)
                  )
                )

                (setq failed (1+ failed))
              )
              (progn
                (princ
                  (strcat
                    "\n[Загружено] "
                    full
                  )
                )

                (setq loaded (1+ loaded))
              )
            )
          )
          (progn
            (princ
              (strcat
                "\n[Пропущено] Модуль не прошёл проверку: "
                full
              )
            )

            (setq failed (1+ failed))
          )
        )
      )
    )
  )

  (princ
    (strcat
      "\nЗагружено модулей: "
      (itoa loaded)
      "\nОшибок / пропущено: "
      (itoa failed)
    )
  )

  (= failed 0)
)

;;; ============================================================
;;; Команды
;;; ============================================================

(defun C:ЗАГРУЗЧИК ()
  (LC:LoadAll)
  (princ)
)

(defun C:LISPLOAD ()
  (LC:LoadAll)
  (princ)
)

(defun C:ПРОВЕРКАМОДУЛЕЙ ()
  (LC:CheckAll)
  (princ)
)

(defun C:LISPCHECK ()
  (LC:CheckAll)
  (princ)
)

(defun C:ПРОВЕРКАФАЙЛА ( / f)
  (setq f
    (getfiled
      "Выберите LSP-файл для проверки"
      ""
      "lsp"
      0
    )
  )

  (if f
    (LC:CheckFile f)
    (princ "\nФайл не выбран.")
  )

  (princ)
)

(defun C:LISPCHECKFILE ( / f)
  (setq f
    (getfiled
      "Выберите LSP-файл для проверки"
      ""
      "lsp"
      0
    )
  )

  (if f
    (LC:CheckFile f)
    (princ "\nФайл не выбран.")
  )

  (princ)
)

;;; ============================================================
;;; Сообщение при загрузке
;;; ============================================================

(princ "\n=============================================")
(princ "\nЗагружено: 00_LoaderChecker.lsp")
(princ "\nКоманды:")
(princ "\n  ЗАГРУЗЧИК        - проверить и загрузить модули")
(princ "\n  ПРОВЕРКАМОДУЛЕЙ  - только проверить модули")
(princ "\n  ПРОВЕРКАФАЙЛА    - проверить выбранный файл")
(princ "\n=============================================")
(princ)