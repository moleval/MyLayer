;;; ============================================================
;;; 02_LayerExport.lsp
;;; Экспорт слоёв и фильтров в Excel-XML (формат 2003).
;;; Формат листа "Фильтры" совместим с 03_LayerImport.lsp:
;;;   Имя фильтра | Список слоев
;;; Состав слоёв фильтров в AutoCAD 2013 через LISP недоступен —
;;; колонка "Список слоев" заполняется в Excel вручную.
;;; Команды:
;;;   МОИСЛОИВЫГРУЗКА / СЛОИВЫГРУЗКА / LAYERS2XML
;;; ============================================================

(vl-load-com)

;;; ============================================================
;;; Базовые функции
;;; ============================================================

(defun LX:IsString (x) (eq (type x) 'STR))

(defun LX:IsError (x)
  (if x (vl-catch-all-error-p x) nil)
)

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
  (while (< n width)
    (setq s (strcat s " "))
    (setq n (1+ n))
  )
  s
)

(defun LX:RepeatChar (c n / s)
  (setq s "")
  (while (> n 0)
    (setq s (strcat s c))
    (setq n (1- n))
  )
  s
)

(defun LX:BoolText (x)
  (cond
    ((null x) "")
    ((eq x T) "Да")
    ((eq x :vlax-true) "Да")
    ((eq x :vlax-false) "Нет")
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
  (if (not (eq (type s) 'STR))
    (setq s (LX:ToStr s))
  )
  (if (or (not find) (= find ""))
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
    (setq row
      (strcat row
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

(defun LX:PropStr (obj prop)
  (LX:ToStr (LX:GetPropValue obj prop))
)

(defun LX:GetFirstPropValue (obj props / val)
  (foreach p props
    (if (null val)
      (setq val (LX:GetPropValue obj p))
    )
  )
  val
)

(defun LX:GetFirstPropStr (obj props / s)
  (foreach p props
    (if (or (not s) (= s ""))
      (setq s (LX:PropStr obj p))
    )
  )
  s
)

(defun LX:HasCount (obj / cnt)
  (setq cnt (LX:GetPropValue obj 'Count))
  (and (numberp cnt) (>= cnt 0))
)

;;; ============================================================
;;; Путь и файл экспорта
;;; ============================================================

(defun LX:OutFile (/ p base)
  (setq p (getvar "DWGPREFIX"))
  (if (and p (/= p ""))
    (if (vl-file-directory-p p)
      (setq p (strcat p "\\"))
      (setq p (strcat (vl-filename-directory p) "\\"))
    )
    (setq p nil)
  )
  (if (or (not p) (= p "") (= p "\\"))
    (progn
      (setq p (getenv "TEMP"))
      (if (not p) (setq p "C:\\Temp"))
      (setq p (strcat p "\\"))
    )
  )
  (setq base (getvar "DWGNAME"))
  (if (or (not base) (= base "")) (setq base "Untitled.dwg"))
  (strcat p (vl-filename-base base) " Слои.xml")
)

;;; ============================================================
;;; TrueColor и прозрачность слоя
;;; ============================================================

(defun LX:GetTrueColorObject (layer / tc tmp)
  (setq tc (LX:GetPropValue layer 'TrueColor))
  (if (and tc (eq (type tc) 'VARIANT))
    (progn
      (setq tmp (vl-catch-all-apply 'vlax-variant-value (list tc)))
      (if (not (LX:IsError tmp))
        (setq tc tmp)
      )
    )
  )
  tc
)

(defun LX:GetTransparency (layer / val obj tmp)
  (setq val (LX:GetFirstPropValue layer '(Transparency LayerTransparency)))
  (if (and val (eq (type val) 'VLA-OBJECT))
    (progn
      (setq obj val)
      (setq val (LX:GetFirstPropValue obj '(Percent Opacity Value Transparency)))
    )
  )
  (if (and val (eq (type val) 'VARIANT))
    (progn
      (setq tmp (vl-catch-all-apply 'vlax-variant-value (list val)))
      (if (not (LX:IsError tmp))
        (setq val tmp)
      )
    )
  )
  val
)

;;; ============================================================
;;; Экспорт слоёв
;;; ============================================================

(defun LX:WriteLayersSheet (fh doc / layers layer name ent flags color
                            desc linetype lw plotflag transparency
                            plotStyle tc colorName red green blue
                            colorMethod row count)
  (write-line "<Worksheet ss:Name=\"Слои\"><Table>" fh)
  (write-line
    (XML:Row
      '("Имя слоя" "Описание" "Цвет ACI" "Цвет имя" "Цвет метод"
        "R" "G" "B" "Тип линии" "Вес линии код" "Вес линии текст"
        "Прозрачность" "Стиль печати" "Включен" "Заморожен"
        "Заморожен в новых ВЭ" "Заблокирован" "Печатается"
        "Зависит от XREF" "Флаги 70"))
    fh
  )

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
  (setq layers (vla-get-Layers doc))

  (vlax-for layer layers
    (setq name (LX:PropStr layer 'Name))
    (setq ent (tblsearch "LAYER" name))

    (if ent
      (progn
        (setq flags    (cdr (assoc 70 ent)))
        (setq color    (cdr (assoc 62 ent)))
        (setq desc     (cdr (assoc 3 ent)))
        (setq linetype (cdr (assoc 6 ent)))
        (setq lw       (cdr (assoc 370 ent)))
        (setq plotflag (cdr (assoc 290 ent)))
      )
      (progn
        (setq flags 0)
        (setq color nil)
        (setq desc nil)
        (setq linetype nil)
        (setq lw nil)
        (setq plotflag nil)
      )
    )

    (if (null flags) (setq flags 0))

    (if (or (not desc) (= desc ""))
      (setq desc (LX:PropStr layer 'Description))
    )

    (if (or (not linetype) (= linetype ""))
      (setq linetype (LX:PropStr layer 'Linetype))
    )

    (if (null lw)
      (setq lw (LX:GetPropValue layer 'Lineweight))
    )

    (setq tc (LX:GetTrueColorObject layer))
    (if tc
      (progn
        (setq colorName   (LX:PropStr tc 'ColorName))
        (setq colorMethod (LX:PropStr tc 'ColorMethod))
        (setq red         (LX:PropStr tc 'Red))
        (setq green       (LX:PropStr tc 'Green))
        (setq blue        (LX:PropStr tc 'Blue))
      )
      (progn
        (setq colorName "")
        (setq colorMethod "")
        (setq red "")
        (setq green "")
        (setq blue "")
      )
    )

    (setq transparency (LX:GetTransparency layer))
    (setq plotStyle (LX:GetFirstPropStr layer '(PlotStyleName PlotStyle)))

    (setq row
      (list
        name
        desc
        (if color (abs color) "")
        colorName
        colorMethod
        red
        green
        blue
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
        flags
      )
    )

    (write-line (XML:Row row) fh)

    (princ (strcat "\n  "
                   (LX:PadRight name 45)         "| "
                   (LX:PadRight (nth  2 row)  6) "| "
                   (LX:PadRight (nth  8 row) 16) "| "
                   (LX:PadRight (nth 10 row) 13) "| "
                   (LX:PadRight (nth 13 row)  5) "| "
                   (LX:PadRight (nth 14 row)  5) "| "
                   (nth 16 row)))

    (setq count (1+ count))
  )

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
        (setq res d)
      )
    )
  )

  (if (not res)
    (progn
      (setq ename
        (vl-catch-all-apply 'dictsearch
                            (list (namedobjdict) name)))
      (if (not (LX:IsError ename))
        (progn
          (if (eq (type ename) 'ENAME)
            (setq res (LX:AsVla ename))
          )
          (if (eq (type ename) 'LIST)
            (progn
              (setq lst (assoc -1 ename))
              (if lst
                (setq res (LX:AsVla (cdr lst)))
              )
            )
          )
        )
      )
    )
  )

  res
)

(defun LX:DictEntries (dict / ename result item name obj)
  (setq result nil)
  (setq ename nil)

  (cond
    ((eq (type dict) 'VLA-OBJECT)
      (setq ename (vl-catch-all-apply 'vlax-vla-object->ename (list dict)))
    )
    ((eq (type dict) 'ENAME)
      (setq ename dict)
    )
  )

  (if (LX:IsError ename)
    (setq ename nil)
  )

  (if ename
    (progn
      (vl-catch-all-apply 'dictsearch (list ename))
      (setq item (vl-catch-all-apply 'dictnext (list ename)))

      (while (and item
                  (not (LX:IsError item))
                  (or (eq (type item) 'LIST)
                      (eq (type item) 'ENAME)
                      (eq (type item) 'VLA-OBJECT)))
        (cond
          ((eq (type item) 'LIST)
            (setq name (cdr (assoc 3 item)))
            (setq obj  (cdr (assoc 350 item)))
            (if (not obj)
              (setq obj (cdr (assoc 360 item)))
            )
            (if (and name obj)
              (setq result (cons (cons name obj) result))
            )
          )
          ((eq (type item) 'ENAME)
            (setq name (LX:PropStr item 'Name))
            (if (= name "")
              (setq name "ITEM")
            )
            (setq result (cons (cons name item) result))
          )
          ((eq (type item) 'VLA-OBJECT)
            (setq name (LX:PropStr item 'Name))
            (if (= name "")
              (setq name "ITEM")
            )
            (setq result (cons (cons name item) result))
          )
        )
        (setq item (vl-catch-all-apply 'dictnext (list ename)))
      )
    )
  )

  (reverse result)
)

(defun LX:DictionaryItems (dictName / dictObj result count i item name)
  (setq dictObj (LX:FindDictionaryObject dictName))

  (if dictObj
    (progn
      (setq count (LX:GetPropValue dictObj 'Count))
      (if (and (numberp count) (> count 0))
        (progn
          (setq count (fix count))
          (setq i 0)
          (repeat count
            (setq item
              (vl-catch-all-apply 'vlax-invoke (list dictObj 'Item i)))
            (if (not (LX:IsError item))
              (progn
                (setq item (LX:AsVla item))
                (setq name (LX:PropStr item 'Name))
                (if (= name "") (setq name (itoa i)))
                (setq result (cons (cons name item) result))
              )
            )
            (setq i (1+ i))
          )
        )
      )
      (if (not result)
        (setq result (LX:DictEntries dictObj))
      )
    )
  )

  (reverse result)
)

(defun LX:GetLayerFilters (/ names items)
  (setq names
    '("ACAD_LAYERFILTERS"
      "ACAD_LAYERFILTER"
      "AcadLayerFilters"
      "ACAD_FILTER"))
  (foreach n names
    (if (not items)
      (setq items (LX:DictionaryItems n))
    )
  )
  items
)

;;; ============================================================
;;; Экспорт фильтров (совместим с 03_LayerImport.lsp)
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
      (setq count (1+ count))
    )
    (progn
      ;; Пустую заглушку не пишем — иначе импорт создаст
      ;; реальный фильтр с именем "Фильтры не найдены".
      (princ "\n  (фильтры не найдены)")))
    )
  )

  (write-line "</Table></Worksheet>" fh)
  (princ "\n")
  count
)

;;; ============================================================
;;; Основная команда экспорта
;;; ============================================================

(defun LX:Export (/ doc fname fh tmpdir layerCount filterCount)
  (vl-load-com)
  (princ "\n")
  (princ "\n=== Экспорт слоёв и фильтров в XML ===")

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq fname (LX:OutFile))

  (princ (strcat "\nЧертёж: " (getvar "DWGNAME")))
  (princ (strcat "\nФайл:   " fname))

  (if (findfile fname)
    (princ "\nФайл существует — перезаписываю.")
  )

  (setq fh (open fname "w"))

  (if (not fh)
    (progn
      (setq tmpdir (getenv "TEMP"))
      (if (not tmpdir) (setq tmpdir "C:\\Temp"))
      (setq fname (strcat tmpdir "\\" (vl-filename-base fname) ".xml"))
      (setq fh (open fname "w"))
      (princ (strcat "\nОсновной путь недоступен. Пишу в: " fname))
    )
  )

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
        fh
      )
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
      (princ "\n")
      (princ "\nПримечание: состав слоёв фильтров в AutoCAD 2013")
      (princ "\nчерез LISP не выгружается. Откройте XML в Excel")
      (princ "\nи заполните колонку \"Список слоев\" вручную —")
      (princ "\nнапример: Витражи,Адаптер алюминиевый,Адаптер ПВХ")
      (princ "\nДля вложенных — >>Имя (например: >>Подсистема)")
      (princ "\n")
    )
    (princ "\nНе удалось создать файл экспорта.")
  )

  (princ)
)

;;; ============================================================
;;; Команды
;;; ============================================================

(defun C:LAYERS2XML ()      (LX:Export))
(defun C:СЛОИВЫГРУЗКА ()    (LX:Export))
(defun C:МОИСЛОИВЫГРУЗКА () (LX:Export))

(princ "\n=============================================")
(princ "\nЗагружено: 02_LayerExport.lsp")
(princ "\nКоманды:")
(princ "\n  МОИСЛОИВЫГРУЗКА")
(princ "\n  СЛОИВЫГРУЗКА")
(princ "\n  LAYERS2XML")
(princ "\n=============================================")
(princ)