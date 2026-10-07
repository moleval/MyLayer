;;; ============================================================
;;; 00_LoaderChecker.lsp
;;;
;;; Модуль-загрузчик и базовая проверка синтаксиса LISP-файлов:
;;; - проверка скобок;
;;; - проверка незакрытых строк (с поддержкой многострочных);
;;; - загрузка модулей только после проверки.
;;;
;;; Команды:
;;; МОИСЛОИЗАГРУЗЧИК         - проверить и загрузить все модули
;;; МОИСЛОИПРОВЕРКАМОДУЛЕЙ   - только проверить все модули
;;; МОИСЛОИПРОВЕРКАФАЙЛА     - проверить выбранный вручную LSP-файл
;;; ============================================================

;;; ========================= НАСТРОЙКИ =========================

;; Папка с модулями.
;; Пусто = поиск через findfile (в путях поддержки AutoCAD).

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
    (if (= (substr p (strlen p) 1) "\\") p (strcat p "\\"))
    p
  )
)

(defun LC:FindFile (name / full)
  (if (and *LC:PATH* (/= *LC:PATH* ""))
    (progn
      (setq full
        (strcat (LC:EnsureSlash *LC:PATH*) name))
      (if (findfile full) full (findfile name)))
    (findfile name)
  )
)

(defun LC:ErrorMessage (err / msg)
  (setq msg
    (vl-catch-all-apply
      'vl-catch-all-error-message
      (list err)))
  (if (or (not msg) (/= (type msg) 'STR))
    "неизвестная ошибка"
    msg)
)

;;; ============================================================
;;; Проверка одного файла
;;; ============================================================

(defun LC:CheckFile
       (fname / fh line lineNo errors stack inString len i c totalLines item)
  (setq fh (open fname "r"))
  (if (not fh)
    (progn
      (princ (strcat "\n[Ошибка] Не удалось открыть файл: " fname))
      nil)
    (progn
      (setq lineNo 0)
      (setq errors 0)
      (setq stack nil)
      (setq inString nil)
      (setq totalLines 0)

      (while (setq line (read-line fh))
        (setq lineNo (1+ lineNo))
        (setq totalLines lineNo)
        (setq len (strlen line))
        (setq i 1)

        (while (<= i len)
          (setq c (substr line i 1))

          (if inString
            (progn
              (if (= c "\\")
                (setq i (1+ i))
                (if (= c "\"")
                  (setq inString nil))))
            (progn
              (cond
                ((= c ";") (setq i len))
                ((= c "\"") (setq inString T))
                ((= c "(") (setq stack (cons (list lineNo line) stack)))
                ((= c ")")
                  (if stack
                    (setq stack (cdr stack))
                    (progn
                      (princ "\n[Ошибка] Лишняя закрывающая скобка:")
                      (princ (strcat "\n  Файл:   " fname))
                      (princ (strcat "\n  Строка: " (itoa lineNo)))
                      (princ (strcat "\n  Текст:  " line))
                      (setq errors (1+ errors))
                    )
                  )
                )
              )
            )
          )

          (setq i (1+ i))
        )
      )

      (close fh)

      (if inString
        (progn
          (princ "\n[Ошибка] Незакрытая строка:")
          (princ (strcat "\n  Файл:   " fname))
          (princ (strcat "\n  Строки: до " (itoa totalLines)))
          (setq errors (1+ errors))
        )
      )

      (if stack
        (progn
          (princ "\n[Ошибка] Незакрытые открывающие скобки:")
          (princ (strcat "\n  Файл:   " fname))
          (foreach item (reverse stack)
            (princ (strcat "\n    стр. " (itoa (car item))
                           ": " (cadr item)))
          )
          (setq errors (1+ errors))
        )
      )

      (if (= errors 0)
        (progn
          (princ (strcat "\n[OK]   " fname
                         "  (" (itoa totalLines) " строк)"))
          T)
        (progn
          (princ (strcat "\n[FAIL] " fname
                         "\n  Найдено ошибок: " (itoa errors)))
          nil)
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
      (princ (strcat "\n[Ошибка] Файл модуля не найден: " name))
      nil)
  )
)

(defun LC:CheckAll (/ allOk moduleOk okCount failCount)
  (setq allOk T)
  (setq okCount 0)
  (setq failCount 0)

  (princ "\n=== Проверка модулей ===")

  (foreach m *LC:MODULES*
    (setq moduleOk (LC:CheckModule m))
    (if moduleOk
      (setq okCount (1+ okCount))
      (progn
        (setq allOk nil)
        (setq failCount (1+ failCount))
      )
    )
  )

  (princ "\n")
  (princ (strcat "\nВсего модулей: " (itoa (length *LC:MODULES*))))
  (princ (strcat "\n  OK:   " (itoa okCount)))
  (princ (strcat "\n  FAIL: " (itoa failCount)))

  (if allOk
    (princ "\nВсе модули прошли проверку.")
    (princ "\nПроверка завершена с ошибками.")
  )

  allOk
)

;;; ============================================================
;;; Загрузка всех модулей
;;; ============================================================

(defun LC:LoadAll (/ full res loaded failed moduleOk m)
  (setq loaded 0)
  (setq failed 0)

  (princ "\n=== Проверка и загрузка модулей ===")

  (foreach m *LC:MODULES*
    (setq full (LC:FindFile m))
    (if (not full)
      (progn
        (princ (strcat "\n[Ошибка] Файл модуля не найден: " m))
        (setq failed (1+ failed)))
      (progn
        (setq moduleOk (LC:CheckFile full))
        (if moduleOk
          (progn
            (setq res (vl-catch-all-apply 'load (list full)))
            (if (vl-catch-all-error-p res)
              (progn
                (princ (strcat "\n[Ошибка] Ошибка загрузки модуля: " full))
                (princ (strcat "\n  " (LC:ErrorMessage res)))
                (setq failed (1+ failed)))
              (progn
                (princ (strcat "\n[Загружено] " full))
                (setq loaded (1+ loaded)))))
          (progn
            (princ (strcat "\n[Пропущено] Модуль не прошёл проверку: " full))
            (setq failed (1+ failed)))))
    )
  )

  (princ "\n")
  (princ (strcat "\nЗагружено модулей: " (itoa loaded)))
  (princ (strcat "\nОшибок / пропущено: " (itoa failed)))

  (= failed 0)
)

;;; ============================================================
;;; Диалог выбора файла
;;; ============================================================

(defun LC:PickFile ( / p dwgdir)
  (setq p (getvar "DWGPREFIX"))
  (if (and p (/= p "") (vl-file-directory-p p))
    (setq dwgdir p)
    (setq dwgdir ""))

  (getfiled
    "Выберите LSP-файл для проверки"
    dwgdir
    "lsp"
    0)
)

;;; ============================================================
;;; Команды
;;; ============================================================

(defun C:МОИСЛОИЗАГРУЗЧИК ()
  (LC:LoadAll)
  (princ)
)

(defun C:МОИСЛОИПРОВЕРКАМОДУЛЕЙ ()
  (LC:CheckAll)
  (princ)
)

(defun C:МОИСЛОИПРОВЕРКАФАЙЛА ( / f)
  (setq f (LC:PickFile))
  (if f
    (LC:CheckFile f)
    (princ "\nФайл не выбран."))
  (princ)
)

;;; ============================================================
;;; Сообщение при загрузке
;;; ============================================================

(princ "\n=============================================")
(princ "\nЗагружено: 00_LoaderChecker.lsp")
(princ "\nКоманды:")
(princ "\n  МОИСЛОИЗАГРУЗЧИК        - проверить и загрузить модули")
(princ "\n  МОИСЛОИПРОВЕРКАМОДУЛЕЙ  - только проверить модули")
(princ "\n  МОИСЛОИПРОВЕРКАФАЙЛА    - проверить выбранный файл")
(princ "\n=============================================")
(princ)