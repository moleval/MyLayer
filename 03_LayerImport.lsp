;;; ============================================================
;;; 03_LayerImport.lsp
;;;
;;; Импорт слоёв из XML-шаблона, созданного модулем
;;; 02_LayerExport.lsp
;;;
;;; Логика:
;;; - если слоя нет - создать;
;;; - если слой есть - обновить свойства;
;;; - шаблон ищется в папке чертежа;
;;; - если шаблон не найден или нужно указать другой файл -
;;;   открывается диалог выбора файла.
;;;
;;; Команды:
;;; СЛОИЗАГРУЗИТЬ
;;; LAYERSLOAD
;;; ============================================================

(vl-load-com)

;; Если nil:
;; - сначала используется шаблон из папки чертежа, если он найден.
;; Если T:
;; - всегда открывается диалог выбора XML-файла.

(setq *LI:ALWAYS-ASK* nil)

;;; ============================================================
;;; Базовые функции
;;; ============================================================

(defun LI:IsString (x)
  (eq (type x) 'STR)
)

(defun LI:IsError (x)
  (if x
    (vl-catch-all-error-p x)
    nil
  )
)

(defun LI:AsVla (x / obj)
  (cond
    ((= (type x) 'VLA-OBJECT)
      x
    )

    ((= (type x) 'ENAME)
      (setq obj
        (vl-catch-all-apply 'vlax-ename->vla-object (list x))
      )

      (if (LI:IsError obj)
        nil
        obj
      )
    )

    (t
      nil
    )
  )
)

(defun LI:Replace (s find rep / pos start out lenf)
  (if (null s)
    (setq s "")
  )

  (if (not (LI:IsString s))
    (setq s (vl-prin1-to-string s))
  )

  (if (or (not find) (= find ""))
    s
    (progn
      (setq lenf (strlen find))
      (setq start 0)
      (setq out "")

      (while (setq pos (vl-string-search find s start))
        (setq out
          (strcat
            out
            (substr s (1+ start) (- pos start))
            rep
          )
        )

        (setq start (+ pos lenf))
      )

      (setq out
        (strcat
          out
          (substr s (1+ start))
        )
      )

      out
    )
  )
)

(defun LI:IsSpaceChar (c)
  (or
    (= c " ")
    (= c (chr 9))
    (= c (chr 10))
    (= c (chr 13))
  )
)

(defun LI:Trim (s / n i)
  (if (null s)
    (setq s "")
  )

  (if (not (LI:IsString s))
    (setq s (vl-prin1-to-string s))
  )

  (setq n (strlen s))
  (setq i 1)

  (while (and
           (<= i n)
           (LI:IsSpaceChar (substr s i 1))
         )
    (setq i (1+ i))
  )

  (setq s (substr s i))

  (setq n (strlen s))

  (while (and
           (> n 0)
           (LI:IsSpaceChar (substr s n 1))
         )
    (setq s (substr s 1 (1- n)))
    (setq n (1- n))
  )

  s
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
  (setq s (LI:Trim s))

  (if (= s "")
    nil
    (progn
      (setq v (distof s))

      (if (numberp v)
        (fix v)
        nil
      )
    )
  )
)

(defun LI:YesNoTrue (s / tstr)
  (setq tstr (strcase (LI:Trim s)))

  (or
    (= tstr "ДА")
    (= tstr "YES")
    (= tstr "TRUE")
    (= tstr "1")
    (= tstr "-1")
    (= tstr "ON")
    (= tstr "ИСТИНА")
  )
)

;;; ============================================================
;;; Чтение XML
;;; ============================================================

(defun LI:ReadFile (fname / fh line txt)
  (setq fh (open fname "r"))

  (if fh
    (progn
      (setq txt "")

      (while (setq line (read-line fh))
        (setq txt (strcat txt line "\n"))
      )

      (close fh)

      txt
    )
    nil
  )
)

(defun LI:GetWorksheet (xml name / pattern pos start end)
  (setq pattern
    (strcat
      "<Worksheet ss:Name=\""
      name
      "\">"
    )
  )

  (setq pos (vl-string-search pattern xml))

  (if pos
    (progn
      (setq start (+ pos (strlen pattern)))
      (setq end (vl-string-search "</Worksheet>" xml start))

      (if end
        (substr xml (1+ start) (- end start))
        nil
      )
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

          (if end
            (substr xml (+ tagEnd 2) (- end tagEnd 1))
            nil
          )
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
            (setq row
              (substr
                sheet
                (+ tagEnd 2)
                (- end tagEnd 1)
              )
            )

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

(defun LI:GetCellValues (row / vals pos tagEnd start0 end val)
  (setq vals nil)
  (setq pos 0)

  (while (setq pos (vl-string-search "<Data" row pos))
    (setq tagEnd (vl-string-search ">" row pos))

    (if tagEnd
      (progn
        (setq start0 (1+ tagEnd))
        (setq end (vl-string-search "</Data>" row start0))

        (if end
          (progn
            (setq val
              (substr
                row
                (1+ start0)
                (- end start0)
              )
            )

            (setq vals
              (append
                vals
                (list (LI:XmlUnescape val))
              )
            )

            (setq pos (+ end 7))
          )
          (setq pos (strlen row))
        )
      )
      (setq pos (strlen row))
    )
  )

  vals
)

;;; ============================================================
;;; Разбор заголовков таблицы
;;; ============================================================

(defun LI:FixedHeaderMap ()
  (list
    (cons "ИМЯ СЛОЯ"                 0)
    (cons "ОПИСАНИЕ"                 1)
    (cons "ЦВЕТ ACI"                 2)
    (cons "ЦВЕТ ИМЯ"                 3)
    (cons "ЦВЕТ МЕТОД"               4)
    (cons "R"                        5)
    (cons "G"                        6)
    (cons "B"                        7)
    (cons "ТИП ЛИНИИ"                8)
    (cons "ВЕС ЛИНИИ КОД"            9)
    (cons "ВЕС ЛИНИИ ТЕКСТ"          10)
    (cons "ПРОЗРАЧНОСТЬ"             11)
    (cons "СТИЛЬ ПЕЧАТИ"             12)
    (cons "ВКЛЮЧЕН"                  13)
    (cons "ЗАМОРОЖЕН"                14)
    (cons "ЗАМОРОЖЕН В НОВЫХ ВЭ"     15)
    (cons "ЗАБЛОКИРОВАН"             16)
    (cons "ПЕЧАТАЕТСЯ"               17)
    (cons "ЗАВИСИТ ОТ XREF"          18)
    (cons "ФЛАГИ 70"                 19)
  )
)

(defun LI:BuildHeaderMap (headers / i map h)
  (setq i 0)
  (setq map nil)

  (foreach h headers
    (setq map
      (append
        map
        (list
          (cons
            (strcase (LI:Trim h))
            i
          )
        )
      )
    )

    (setq i (1+ i))
  )

  ;; Если заголовки распознались нормально, используем их.
  ;; Если нет - используем фиксированное расположение колонок.

  (if (assoc "ИМЯ СЛОЯ" map)
    map
    (LI:FixedHeaderMap)
  )
)

(defun LI:GetByHeader (vals map header / idx)
  (setq idx
    (cdr
      (assoc
        (strcase (LI:Trim header))
        map
      )
    )
  )

  (if (and idx (< idx (length vals)))
    (nth idx vals)
    ""
  )
)

;;; ============================================================
;;; Работа со слоями
;;; ============================================================

(defun LI:EnsureLayer (doc name / layers res)
  (if (tblsearch "LAYER" name)
    T
    (progn
      (setq layers (vla-get-Layers doc))

      (setq res
        (vl-catch-all-apply
          'vla-add
          (list layers name)
        )
      )

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
    (logand flags (lognot bit))
  )
)

(defun LI:LoadLinetype (doc name / ltypes res)
  (if (and
        name
        (/= name "")
        (not (tblsearch "LTYPE" name))
      )
    (progn
      (setq ltypes
        (vl-catch-all-apply
          'vla-get-Linetypes
          (list doc)
        )
      )

      (if (not (LI:IsError ltypes))
        (progn
          (setq res
            (vl-catch-all-apply
              'vlax-invoke
              (list ltypes 'Load name)
            )
          )

          (if (LI:IsError res)
            (setq res
              (vl-catch-all-apply
                'vlax-invoke
                (list ltypes 'Load name "")
              )
            )
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
      (setq res
        (vl-catch-all-apply
          'vlax-put
          (list layer 'Transparency num)
        )
      )

      (if (LI:IsError res)
        (progn
          (setq obj
            (vl-catch-all-apply
              'vlax-get
              (list layer 'Transparency)
            )
          )

          (if (not (LI:IsError obj))
            (progn
              (setq res
                (vl-catch-all-apply
                  'vlax-put
                  (list obj 'Percent num)
                )
              )

              (if (LI:IsError res)
                (vl-catch-all-apply
                  'vlax-put
                  (list obj 'Value num)
                )
              )
            )
          )
        )
      )
    )
  )
)

(defun LI:SetPlotStyle (layer val / res)
  (setq res
    (vl-catch-all-apply
      'vlax-put
      (list layer 'PlotStyleName val)
    )
  )

  (if (LI:IsError res)
    (vl-catch-all-apply
      'vlax-put
      (list layer 'PlotStyle val)
    )
  )
)

(defun LI:ApplyLayerRow
       (
        doc
        vals
        map
        /
        name
        isNew
        tbl
        ent
        flags
        aci
        onStr
        on
        oldColor
        colorVal
        desc
        lt
        lw
        plotStr
        frozen
        frozenVp
        locked
        res
        layerObj
        transStr
        plotStyle
       )

  (setq name
    (LI:Trim
      (LI:GetByHeader vals map "Имя слоя")
    )
  )

  (if (= name "")
    nil
    (progn
      (setq isNew
        (not
          (tblsearch "LAYER" name)
        )
      )

      (if (not (LI:EnsureLayer doc name))
        (progn
          (princ
            (strcat
              "\n[Ошибка] Не удалось создать слой: "
              name
            )
          )

          "Ошибка"
        )
        (progn
          (setq tbl (tblsearch "LAYER" name))

          (if (not tbl)
            (progn
              (princ
                (strcat
                  "\n[Ошибка] Не найдена запись слоя: "
                  name
                )
              )

              "Ошибка"
            )
            (progn
              (setq ent
                (entget
                  (cdr (assoc -1 tbl))
                )
              )

              ;; Описание слоя

              (setq desc
                (LI:Trim
                  (LI:GetByHeader vals map "Описание")
                )
              )

              (if (/= desc "")
                (setq ent (LI:SetDxf ent 3 desc))
              )

              ;; Тип линии

              (setq lt
                (LI:Trim
                  (LI:GetByHeader vals map "Тип линии")
                )
              )

              (if (/= lt "")
                (progn
                  (if (not (tblsearch "LTYPE" lt))
                    (LI:LoadLinetype doc lt)
                  )

                  (if (tblsearch "LTYPE" lt)
                    (setq ent (LI:SetDxf ent 6 lt))
                    (progn
                      (if (tblsearch "LTYPE" "Continuous")
                        (progn
                          (setq ent (LI:SetDxf ent 6 "Continuous"))

                          (princ
                            (strcat
                              "\n[Предупреждение] Тип линии не найден: "
                              lt
                              ". Назначен Continuous."
                            )
                          )
                        )
                        (princ
                          (strcat
                            "\n[Предупреждение] Тип линии не найден: "
                            lt
                          )
                        )
                      )
                    )
                  )
                )
              )

              ;; Цвет и включён/выключен

              (setq aci
                (LI:ToInt
                  (LI:GetByHeader vals map "Цвет ACI")
                )
              )

              (setq onStr
                (LI:Trim
                  (LI:GetByHeader vals map "Включен")
                )
              )

              (if (= onStr "")
                (setq on T)
                (setq on (LI:YesNoTrue onStr))
              )

              (if (and aci (>= aci 1) (<= aci 255))
                (progn
                  (setq colorVal (abs aci))

                  (if (not on)
                    (setq colorVal (- colorVal))
                  )

                  (setq ent (LI:SetDxf ent 62 colorVal))
                )
                (progn
                  (setq oldColor (cdr (assoc 62 ent)))

                  (if (and oldColor (/= oldColor 0))
                    (progn
                      (if on
                        (setq colorVal (abs oldColor))
                        (setq colorVal (- (abs oldColor)))
                      )

                      (setq ent (LI:SetDxf ent 62 colorVal))
                    )
                  )
                )
              )

              ;; Вес линии

              (setq lw
                (LI:ToInt
                  (LI:GetByHeader vals map "Вес линии код")
                )
              )

              (if (numberp lw)
                (setq ent (LI:SetDxf ent 370 lw))
              )

              ;; Флаги слоя

              (setq flags
                (if (cdr (assoc 70 ent))
                  (cdr (assoc 70 ent))
                  0
                )
              )

              (setq frozen
                (LI:YesNoTrue
                  (LI:GetByHeader vals map "Заморожен")
                )
              )

              (setq frozenVp
                (LI:YesNoTrue
                  (LI:GetByHeader vals map "Заморожен в новых ВЭ")
                )
              )

              (setq locked
                (LI:YesNoTrue
                  (LI:GetByHeader vals map "Заблокирован")
                )
              )

              (setq flags (LI:SetBit flags 1 frozen))
              (setq flags (LI:SetBit flags 2 frozenVp))
              (setq flags (LI:SetBit flags 4 locked))

              (setq ent (LI:SetDxf ent 70 flags))

              ;; Печать

              (setq plotStr
                (LI:Trim
                  (LI:GetByHeader vals map "Печатается")
                )
              )

              (if (/= plotStr "")
                (setq ent
                  (LI:SetDxf
                    ent
                    290
                    (if (LI:YesNoTrue plotStr) 1 0)
                  )
                )
              )

              ;; Применяем изменения

              (setq res
                (vl-catch-all-apply
                  'entmod
                  (list ent)
                )
              )

              (if (LI:IsError res)
                (progn
                  (princ
                    (strcat
                      "\n[Ошибка] Не удалось изменить слой: "
                      name
                    )
                  )

                  "Ошибка"
                )
                (progn
                  (setq layerObj
                    (LI:AsVla
                      (cdr (assoc -1 tbl))
                    )
                  )

                  (if layerObj
                    (progn
                      ;; Прозрачность

                      (setq transStr
                        (LI:Trim
                          (LI:GetByHeader vals map "Прозрачность")
                        )
                      )

                      (if (/= transStr "")
                        (LI:SetTransparency layerObj transStr)
                      )

                      ;; Стиль печати

                      (setq plotStyle
                        (LI:Trim
                          (LI:GetByHeader vals map "Стиль печати")
                        )
                      )

                      (if (/= plotStyle "")
                        (LI:SetPlotStyle layerObj plotStyle)
                      )
                    )
                  )

                  (if isNew
                    (princ
                      (strcat
                        "\n[Создан] "
                        name
                      )
                    )
                    (princ
                      (strcat
                        "\n[Обновлён] "
                        name
                      )
                    )
                  )

                  (if isNew
                    "Создан"
                    "Обновлён"
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
;;; Импорт из файла
;;; ============================================================

(defun LI:ImportFromFile
       (
        fname
        /
        doc
        xml
        sheet
        rows
        headers
        map
        row
        vals
        status
        created
        updated
        errors
        oldLayer
       )

  (setq created 0)
  (setq updated 0)
  (setq errors 0)

  (setq xml (LI:ReadFile fname))

  (if (not xml)
    (princ
      (strcat
        "\nНе удалось прочитать файл: "
        fname
      )
    )
    (progn
      (setq doc
        (vla-get-ActiveDocument
          (vlax-get-acad-object)
        )
      )

      ;; Временно переводим текущий слой на 0,
      ;; чтобы не было запрета на заморозку/выключение
      ;; текущего слоя.

      (setq oldLayer (getvar "CLAYER"))

      (vl-catch-all-apply
        'setvar
        (list "CLAYER" "0")
      )

      (setq sheet (LI:GetWorksheet xml "Слои"))

      (if (not sheet)
        (setq sheet (LI:GetFirstWorksheet xml))
      )

      (if (not sheet)
        (princ "\nНе найден лист слоёв в XML-файле.")
        (progn
          (setq rows (LI:GetRows sheet))

          (if (< (length rows) 2)
            (princ "\nВ файле нет данных слоёв.")
            (progn
              (setq headers
                (LI:GetCellValues (car rows))
              )

              (setq map
                (LI:BuildHeaderMap headers)
              )

              (foreach row (cdr rows)
                (setq vals (LI:GetCellValues row))

                (if vals
                  (progn
                    (setq status
                      (LI:ApplyLayerRow doc vals map)
                    )

                    (cond
                      ((= status "Создан")
                        (setq created (1+ created))
                      )

                      ((= status "Обновлён")
                        (setq updated (1+ updated))
                      )

                      ((= status "Ошибка")
                        (setq errors (1+ errors))
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )

      ;; Возвращаем предыдущий текущий слой, если это возможно.

      (if (and oldLayer (tblsearch "LAYER" oldLayer))
        (vl-catch-all-apply
          'setvar
          (list "CLAYER" oldLayer)
        )
      )

      (princ
        (strcat
          "\nГотово."
          "\nСоздано слоёв: "
          (itoa created)
          "\nОбновлено слоёв: "
          (itoa updated)
          "\nОшибок: "
          (itoa errors)
        )
      )
    )
  )

  (princ)
)

;;; ============================================================
;;; Поиск и выбор файла шаблона
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

  (if (or (not p) (= p "") (= p "\\"))
    nil
    p
  )
)

(defun LI:LocalTemplateFile (/ dir base candidates f result)
  (setq dir (LI:DrawingDir))

  (if dir
    (progn
      (setq base (getvar "DWGNAME"))

      (if (or (not base) (= base ""))
        (setq base "Untitled.dwg")
      )

      (setq candidates
        (list
          (strcat
            dir
            (vl-filename-base base)
            " Слои.xml"
          )

          (strcat
            dir
            "Слои.xml"
          )
        )
      )

      (foreach f candidates
        (if (and
              (not result)
              (findfile f)
            )
          (setq result f)
        )
      )

      result
    )
    nil
  )
)

(defun LI:FindTemplateFile (/ f)
  (setq f (LI:LocalTemplateFile))

  ;; Дополнительно можно поискать Слои.xml
  ;; в путях поддержки файлов, если они заданы в AutoCAD.

  (if (not f)
    (setq f (findfile "Слои.xml"))
  )

  f
)

(defun LI:SelectTemplateFile (/ initial f)
  (setq initial (LI:FindTemplateFile))

  (if (not initial)
    (progn
      (setq initial (LI:DrawingDir))

      (if (not initial)
        (setq initial "")
      )
    )
  )

  (setq f
    (getfiled
      "Выберите XML-шаблон слоёв"
      initial
      "xml"
      0
    )
  )

  f
)

;;; ============================================================
;;; Универсальная команда запуска
;;; ============================================================

(defun LI:Run (/ fname)
  (setq fname (LI:FindTemplateFile))

  (if (and fname (not *LI:ALWAYS-ASK*))
    (princ
      (strcat
        "\nИспользуется шаблон: "
        fname
      )
    )
    (progn
      (if (not fname)
        (princ "\nШаблон слоёв в папке чертежа не найден.")
      )

      (setq fname (LI:SelectTemplateFile))
    )
  )

  (if fname
    (LI:ImportFromFile fname)
    (princ "\nФайл шаблона не выбран.")
  )

  (princ)
)

;;; ============================================================
;;; Команды
;;; ============================================================

(defun C:СЛОИЗАГРУЗИТЬ ()
  (LI:Run)
)

(defun C:LAYERSLOAD ()
  (LI:Run)
)

(princ "\n=============================================")
(princ "\nЗагружено: 03_LayerImport.lsp")
(princ "\nКоманды:")
(princ "\n  СЛОИЗАГРУЗИТЬ")
(princ "\n  LAYERSLOAD")
(princ "\n=============================================")
(princ)