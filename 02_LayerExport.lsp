;;; ============================================================
;;; 02_LayerExport.lsp
;;;
;;; Модуль экспорта данных из текущего DWG:
;;; - все слои и их свойства;
;;; - фильтры слоёв / групповые фильтры, если они доступны.
;;;
;;; Результат:
;;; ИмяЧертёжа Слои.xml
;;;
;;; Команды:
;;; МОИСЛОИВЫГРУЗКА
;;; СЛОИВЫГРУЗКА
;;; LAYERS2XML
;;; ============================================================

(vl-load-com)

;;; ============================================================
;;; Базовые функции
;;; ============================================================

(defun LX:IsString (x)
  (eq (type x) 'STR)
)

(defun LX:IsError (x)
  (if x
    (vl-catch-all-error-p x)
    nil
  )
)

(defun LX:ListToString (lst / s)
  (foreach x lst
    (setq s
      (strcat
        s
        (if s "|" "")
        (LX:ToStr (LX:Normalize x))
      )
    )
  )

  (if s s "")
)

(defun LX:Normalize (val / tmp)
  (cond
    ((null val)
      ""
    )

    ((eq (type val) 'VARIANT)
      (setq tmp
        (vl-catch-all-apply 'vlax-variant-value (list val))
      )

      (if (LX:IsError tmp)
        (vl-prin1-to-string val)
        (LX:Normalize tmp)
      )
    )

    ((eq (type val) 'SAFEARRAY)
      (setq tmp
        (vl-catch-all-apply 'vlax-safearray->list (list val))
      )

      (if (LX:IsError tmp)
        (vl-prin1-to-string val)
        (LX:ListToString tmp)
      )
    )

    ((listp val)
      (LX:ListToString val)
    )

    (t
      val
    )
  )
)

(defun LX:ToStr (x)
  (cond
    ((null x)
      ""
    )

    ((LX:IsString x)
      x
    )

    ((= (type x) 'INT)
      (itoa x)
    )

    ((= (type x) 'REAL)
      (rtos x 2 8)
    )

    ((= (type x) 'VLA-OBJECT)
      ""
    )

    (t
      (vl-prin1-to-string x)
    )
  )
)

(defun LX:AsVla (x / obj)
  (cond
    ((= (type x) 'VLA-OBJECT)
      x
    )

    ((= (type x) 'ENAME)
      (setq obj
        (vl-catch-all-apply 'vlax-ename->vla-object (list x))
      )

      (if (LX:IsError obj)
        nil
        obj
      )
    )

    (t
      nil
    )
  )
)

(defun LX:GetPropValue (obj prop / res)
  (setq obj (LX:AsVla obj))

  (if (not obj)
    nil
    (progn
      (setq res
        (vl-catch-all-apply 'vlax-get (list obj prop))
      )

      (if (LX:IsError res)
        nil
        res
      )
    )
  )
)

(defun LX:PropStr (obj prop)
  (LX:ToStr
    (LX:Normalize
      (LX:GetPropValue obj prop)
    )
  )
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
  (setq cnt
    (LX:Normalize
      (LX:GetPropValue obj 'Count)
    )
  )

  (and
    (numberp cnt)
    (>= cnt 0)
  )
)

(defun LX:BoolText (x)
  (cond
    ((null x)
      ""
    )

    ((eq x T)
      "Да"
    )

    ((eq x :vlax-true)
      "Да"
    )

    ((eq x :vlax-false)
      "Нет"
    )

    ((numberp x)
      (if (= x 0)
        "Нет"
        "Да"
      )
    )

    (t
      "Да"
    )
  )
)

(defun LX:LineweightText (w)
  (cond
    ((null w)
      ""
    )

    ((not (numberp w))
      (LX:ToStr w)
    )

    ((= w -3)
      "По умолчанию"
    )

    ((= w -2)
      "ByBlock"
    )

    ((= w -1)
      "ByLayer"
    )

    (t
      (strcat
        (vl-prin1-to-string (/ w 100.0))
        " мм"
      )
    )
  )
)

;;; ============================================================
;;; XML-функции
;;; ============================================================

(defun XML:Replace (s find rep / pos start out lenf)
  (if (not (LX:IsString s))
    (setq s (LX:ToStr s))
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

(defun XML:Escape (s)
  (setq s (LX:ToStr (LX:Normalize s)))

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
      (strcat
        row
        "<Cell><Data ss:Type=\"String\">"
        s
        "</Data></Cell>"
      )
    )
  )

  (strcat row "</Row>")
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

      (if (not p)
        (setq p "C:\\Temp")
      )

      (setq p (strcat p "\\"))
    )
  )

  (setq base (getvar "DWGNAME"))

  (if (or (not base) (= base ""))
    (setq base "Untitled.dwg")
  )

  (strcat
    p
    (vl-filename-base base)
    " Слои.xml"
  )
)

;;; ============================================================
;;; TrueColor и прозрачность слоя
;;; ============================================================

(defun LX:GetTrueColorObject (layer / tc tmp)
  (setq tc (LX:GetPropValue layer 'TrueColor))

  (if (and tc (eq (type tc) 'VARIANT))
    (progn
      (setq tmp
        (vl-catch-all-apply 'vlax-variant-value (list tc))
      )

      (if (not (LX:IsError tmp))
        (setq tc tmp)
      )
    )
  )

  tc
)

(defun LX:GetTransparency (layer / val obj tmp)
  (setq val
    (LX:GetFirstPropValue
      layer
      '(Transparency LayerTransparency)
    )
  )

  (if (and val (eq (type val) 'VLA-OBJECT))
    (progn
      (setq obj val)

      (setq val
        (LX:GetFirstPropValue
          obj
          '(Percent Opacity Value Transparency)
        )
      )
    )
  )

  (if (and val (eq (type val) 'VARIANT))
    (progn
      (setq tmp
        (vl-catch-all-apply 'vlax-variant-value (list val))
      )

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
                             colorMethod row)

  (write-line "<Worksheet ss:Name=\"Слои\"><Table>" fh)

  (write-line
    (XML:Row
      '(
        "Имя слоя"
        "Описание"
        "Цвет ACI"
        "Цвет имя"
        "Цвет метод"
        "R"
        "G"
        "B"
        "Тип линии"
        "Вес линии код"
        "Вес линии текст"
        "Прозрачность"
        "Стиль печати"
        "Включен"
        "Заморожен"
        "Заморожен в новых ВЭ"
        "Заблокирован"
        "Печатается"
        "Зависит от XREF"
        "Флаги 70"
       )
    )
    fh
  )

  (setq layers (vla-get-Layers doc))

  (vlax-for layer layers
    (setq name (LX:PropStr layer 'Name))

    (setq ent (tblsearch "LAYER" name))

    (if ent
      (progn
        (setq flags (cdr (assoc 70 ent)))
        (setq color (cdr (assoc 62 ent)))
        (setq desc  (cdr (assoc 3 ent)))
        (setq linetype (cdr (assoc 6 ent)))
        (setq lw (cdr (assoc 370 ent)))
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

    (if (null flags)
      (setq flags 0)
    )

    ;; Описание слоя

    (if (or (not desc) (= desc ""))
      (setq desc (LX:PropStr layer 'Description))
    )

    ;; Тип линии

    (if (or (not linetype) (= linetype ""))
      (setq linetype (LX:PropStr layer 'Linetype))
    )

    ;; Вес линии

    (if (null lw)
      (setq lw (LX:GetPropValue layer 'Lineweight))
    )

    ;; TrueColor

    (setq tc (LX:GetTrueColorObject layer))

    (if tc
      (progn
        (setq colorName (LX:PropStr tc 'ColorName))
        (setq colorMethod (LX:PropStr tc 'ColorMethod))
        (setq red (LX:PropStr tc 'Red))
        (setq green (LX:PropStr tc 'Green))
        (setq blue (LX:PropStr tc 'Blue))
      )
      (progn
        (setq colorName "")
        (setq colorMethod "")
        (setq red "")
        (setq green "")
        (setq blue "")
      )
    )

    ;; Прозрачность

    (setq transparency (LX:GetTransparency layer))

    ;; Стиль печати

    (setq plotStyle
      (LX:GetFirstPropStr
        layer
        '(PlotStyleName PlotStyle)
      )
    )

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

        (if (and color (< color 0))
          "Нет"
          "Да"
        )

        (if (= 1 (logand flags 1))
          "Да"
          "Нет"
        )

        (if (= 2 (logand flags 2))
          "Да"
          "Нет"
        )

        (if (= 4 (logand flags 4))
          "Да"
          "Нет"
        )

        (if (null plotflag)
          "Да"
          (LX:BoolText plotflag)
        )

        (if (= 16 (logand flags 16))
          "Да"
          "Нет"
        )

        flags
      )
    )

    (write-line (XML:Row row) fh)
  )

  (write-line "</Table></Worksheet>" fh)
)

;;; ============================================================
;;; Словари и фильтры слоёв
;;; ============================================================

(defun LX:FindDictionaryObject (name / doc dicts d res ename)
  (setq doc
    (vla-get-ActiveDocument
      (vlax-get-acad-object)
    )
  )

  (setq dicts
    (vl-catch-all-apply
      'vlax-get
      (list doc 'Dictionaries)
    )
  )

  (if (and
        (not (LX:IsError dicts))
        (LX:HasCount dicts)
      )
    (vlax-for d dicts
      (if (and
            (not res)
            (=
              (strcase (LX:PropStr d 'Name))
              (strcase name)
            )
          )
        (setq res d)
      )
    )
  )

  (if (not res)
    (progn
      (setq ename
        (vl-catch-all-apply
          'dictsearch
          (list (namedobjdict) name)
        )
      )

      (if (not (LX:IsError ename))
        (cond
          ((= (type ename) 'ENAME)
            (setq res (LX:AsVla ename))
          )

          ((= (type ename) 'LIST)
            (if (assoc -1 ename)
              (setq res
                (LX:AsVla
                  (cdr (assoc -1 ename))
                )
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
  (if (= (type dict) 'VLA-OBJECT)
    (setq ename
      (vl-catch-all-apply
        'vlax-vla-object->ename
        (list dict)
      )
    )
    (setq ename dict)
  )

  (if (and
        ename
        (not (LX:IsError ename))
        (= (type ename) 'ENAME)
      )
    (progn
      (vl-catch-all-apply 'dictsearch (list ename))

      (setq item
        (vl-catch-all-apply 'dictnext (list ename))
      )

      (while (and
               item
               (not (LX:IsError item))
               (or
                 (= (type item) 'LIST)
                 (= (type item) 'ENAME)
                 (= (type item) 'VLA-OBJECT)
               )
             )

        (cond
          ((= (type item) 'LIST)
            (setq name (cdr (assoc 3 item)))

            (setq obj (cdr (assoc 350 item)))

            (if (not obj)
              (setq obj (cdr (assoc 360 item)))
            )

            (if (and name obj)
              (setq result
                (cons (cons name obj) result)
              )
            )
          )

          ((= (type item) 'ENAME)
            (setq name (LX:PropStr item 'Name))

            (if (= name "")
              (setq name "ITEM")
            )

            (setq result
              (cons (cons name item) result)
            )
          )

          ((= (type item) 'VLA-OBJECT)
            (setq name (LX:PropStr item 'Name))

            (if (= name "")
              (setq name "ITEM")
            )

            (setq result
              (cons (cons name item) result)
            )
          )
        )

        (setq item
          (vl-catch-all-apply 'dictnext (list ename))
        )
      )
    )
  )

  (reverse result)
)

(defun LX:DictionaryItems (dictName / dictObj result count i item name)
  (setq dictObj (LX:FindDictionaryObject dictName))

  (if dictObj
    (progn
      (setq count
        (LX:Normalize
          (LX:GetPropValue dictObj 'Count)
        )
      )

      (if (and
            (numberp count)
            (> count 0)
          )
        (progn
          (setq count (fix count))
          (setq i 0)

          (repeat count
            (setq item
              (vl-catch-all-apply
                'vlax-invoke
                (list dictObj 'Item i)
              )
            )

            (if (not (LX:IsError item))
              (progn
                (setq item (LX:AsVla item))

                (setq name (LX:PropStr item 'Name))

                (if (= name "")
                  (setq name (itoa i))
                )

                (setq result
                  (cons (cons name item) result)
                )
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
    '(
      "ACAD_LAYERFILTERS"
      "ACAD_LAYERFILTER"
      "AcadLayerFilters"
      "ACAD_FILTER"
     )
  )

  (foreach n names
    (if (not items)
      (setq items (LX:DictionaryItems n))
    )
  )

  items
)

;;; ============================================================
;;; Экспорт фильтров
;;; ============================================================

(defun LX:WriteFiltersSheet (fh / items item key obj row)
  (write-line "<Worksheet ss:Name=\"Фильтры\"><Table>" fh)

  (write-line
    (XML:Row
      '(
        "Имя фильтра"
        "Тип объекта"
        "Описание"
        "Тип"
        "Тип фильтра"
        "Фильтр"
        "Выражение"
       )
    )
    fh
  )

  (setq items (LX:GetLayerFilters))

  (if items
    (foreach item items
      (setq key (car item))
      (setq obj (cdr item))

      (setq obj (LX:AsVla obj))

      (setq row
        (list
          key
          (LX:PropStr obj 'ObjectName)
          (LX:PropStr obj 'Description)
          (LX:PropStr obj 'Type)
          (LX:PropStr obj 'FilterType)
          (LX:PropStr obj 'Filter)
          (LX:PropStr obj 'Expression)
        )
      )

      (write-line (XML:Row row) fh)
    )

    (write-line
      (XML:Row
        '(
          "Фильтры не найдены"
          ""
          ""
          ""
          ""
          ""
          ""
         )
      )
      fh
    )
  )

  (write-line "</Table></Worksheet>" fh)
)

;;; ============================================================
;;; Основная команда экспорта
;;; ============================================================

(defun LX:Export (/ doc fname fh tmpdir)
  (vl-load-com)

  (setq doc
    (vla-get-ActiveDocument
      (vlax-get-acad-object)
    )
  )

  (setq fname (LX:OutFile))

  (setq fh (open fname "w"))

  (if (not fh)
    (progn
      (setq tmpdir (getenv "TEMP"))

      (if (not tmpdir)
        (setq tmpdir "C:\\Temp")
      )

      (setq fname
        (strcat
          tmpdir
          "\\"
          (vl-filename-base fname)
          ".xml"
        )
      )

      (setq fh (open fname "w"))
    )
  )

  (if fh
    (progn
      ;; Обязательный XML-заголовок
      (write-line
        "<?xml version=\"1.0\" encoding=\"windows-1251\"?>"
        fh
      )

      ;; Эта инструкция говорит Windows/Excel,
      ;; что файл нужно открывать как таблицу.
      ;; Без неё Excel 2013 может выдать ошибку издателя.
      (write-line
        "<?mso-application progid=\"Excel.Sheet\"?>"
        fh
      )

      (write-line
        (strcat
          "<Workbook "
          "xmlns=\"urn:schemas-microsoft-com:office:spreadsheet\" "
          "xmlns:o=\"urn:schemas-microsoft-com:office:office\" "
          "xmlns:x=\"urn:schemas-microsoft-com:office:excel\" "
          "xmlns:ss=\"urn:schemas-microsoft-com:office:spreadsheet\" "
          "xmlns:html=\"http://www.w3.org/TR/REC-html40\">"
        )
        fh
      )

      ;; Блок Styles обязателен для корректного открытия
      ;; в Excel 2013.
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

      (LX:WriteLayersSheet fh doc)
      (LX:WriteFiltersSheet fh)

      (write-line "</Workbook>" fh)

      (close fh)

      (princ
        (strcat
          "\nЭкспорт завершён."
          "\nФайл: "
          fname
        )
      )
    )
    (princ
      "\nНе удалось создать файл экспорта."
    )
  )

  (princ)
)

;;; ============================================================
;;; Команды
;;; ============================================================

(defun C:LAYERS2XML ()
  (LX:Export)
)

(defun C:СЛОИВЫГРУЗКА ()
  (LX:Export)
)

(defun C:МОИСЛОИВЫГРУЗКА ()
  (LX:Export)
)

(princ "\n=============================================")
(princ "\nЗагружено: 02_LayerExport.lsp")
(princ "\nКоманды:")
(princ "\n  МОИСЛОИВЫГРУЗКА")
(princ "\n  СЛОИВЫГРУЗКА")
(princ "\n  LAYERS2XML")
(princ "\n=============================================")
(princ)