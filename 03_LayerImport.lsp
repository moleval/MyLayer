;;; ============================================================
;;; 03_LayerImport.lsp
;;;
;;; Импорт слоёв и фильтров слоёв из XML-шаблона.
;;;
;;; Логика:
;;; - если слоя нет - создать;
;;; - если слой есть - обновить свойства;
;;; - импорт групповых фильтров через команду -LAYER;
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
    ;; Если бит установлен, вычитаем его значение, чтобы сбросить
    (if (= (logand flags bit) bit)
      (- flags bit)
      flags
    )
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

;;; ============================================================
;;; Применение данных одного слоя
;;; ============================================================

(defun LI:ApplyLayerRow
       (
        doc
        vals
        map
        /
        name
        isNew
        ename
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
          ;; Получаем ename слоя напрямую через tblobjname.
          ;; Это исправляет ошибку, когда ассоциативный список
          ;; от tblsearch не содержит ключа -1.

          (setq ename (tblobjname "LAYER" name))

          (if (not ename)
            (progn
              (princ
                (strcat
                  "\n[Ошибка] Не найден объект слоя: "
                  name
                )
              )

              "Ошибка"
            )
            (progn
              (setq ent (entget ename))

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
                    (LI:AsVla ename)
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
;;; Импорт фильтров слоёв (Ultra-Safe Version)
;;; ============================================================

;; Абсолютно безопасное преобразование в строку.
(defun LI:ForceString (x)
  (cond
    ((null x) "")
    ((eq (type x) 'STR) x)
    (t (vl-prin1-to-string x))
  )
)

;; Безопасный Trim
(defun LI:SafeTrim (s)
  (setq s (LI:ForceString s))
  (if (= s "")
    ""
    (progn
      (while (and (> (strlen s) 0) (LI:IsSpaceChar (substr s 1 1)))
        (setq s (substr s 2))
      )
      (while (and (> (strlen s) 0) (LI:IsSpaceChar (substr s (strlen s) 1)))
        (setq s (substr s 1 (1- (strlen s))))
      )
      s
    )
  )
)

;; Безопасное разбиение строки
(defun LI:SafeSplit (s delim / tokens pos start lenf)
  (setq s (LI:ForceString s))
  (setq delim (LI:ForceString delim))
  (if (or (= s "") (= delim ""))
    nil
    (progn
      (setq tokens nil start 0 lenf (strlen delim))
      (while (setq pos (vl-string-search delim s start))
        (setq tokens (append tokens (list (LI:SafeTrim (substr s (1+ start) (- pos start))))))
        (setq start (+ pos lenf))
      )
      (setq tokens (append tokens (list (LI:SafeTrim (substr s (1+ start))))))
      tokens
    )
  )
)

;; Безопасное добавление уникальной строки
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

;; Безопасный поиск определения фильтра
(defun LI:SafeGetFilterDef (defs name / found d)
  (setq name (strcase (LI:SafeTrim name)))
  (foreach d defs
    (if (and (listp d) (car d) (= (strcase (LI:ForceString (car d))) name))
      (setq found d)
    )
  )
  found
)

;; Безопасное преобразование списка в строку через запятую
(defun LI:SafeListToComma (lst / s txt x)
  (foreach x lst
    (setq txt (LI:SafeTrim x))
    (if (/= txt "")
      (setq s (strcat s (if s "," "") txt))
    )
  )
  (if s s "")
)

;; Безопасное определение ссылки на фильтр
(defun LI:SafeGetFilterRef (token / up pos)
  (setq token (LI:SafeTrim token))

  ;; Удаляем возможные кавычки/апострофы слева
  (while (and
           (> (strlen token) 1)
           (or (= (substr token 1 1) "\"")
               (= (substr token 1 1) "'"))
         )
    (setq token (LI:SafeTrim (substr token 2)))
  )

  ;; Удаляем возможные кавычки/апострофы справа
  (while (and
           (> (strlen token) 1)
           (or (= (substr token (strlen token) 1) "\"")
               (= (substr token (strlen token) 1) "'"))
         )
    (setq token (LI:SafeTrim (substr token 1 (1- (strlen token)))))
  )

  (setq up (strcase token))

  (cond
    ;; @ИмяФильтра
    ((and
       (setq pos (vl-string-search "@" token))
       (<= pos 5)
     )
      (LI:SafeTrim
        (substr token (+ pos (strlen "@") 1))
      )
    )

    ;; >>ИмяФильтра
    ((and
       (setq pos (vl-string-search ">>" token))
       (<= pos 5)
     )
      (LI:SafeTrim
        (substr token (+ pos (strlen ">>") 1))
      )
    )

    ;; FILTER:ИмяФильтра
    ((and
       (setq pos (vl-string-search "FILTER:" up))
       (<= pos 5)
     )
      (LI:SafeTrim
        (substr token (+ pos (strlen "FILTER:") 1))
      )
    )

    ;; ФИЛЬТР:ИмяФильтра
    ((and
       (setq pos (vl-string-search "ФИЛЬТР:" up))
       (<= pos 5)
     )
      (LI:SafeTrim
        (substr token (+ pos (strlen "ФИЛЬТР:") 1))
      )
    )

    (t
      nil
    )
  )
)

;; Рекурсивное раскрытие ссылок (с защитой от nil и циклов)
(defun LI:SafeResolve (name defs visited / def raw tokens result token ref childLayers)
  (setq name (LI:SafeTrim name))
  (if (member (strcase name) visited)
    nil
    (progn
      (setq visited (cons (strcase name) visited))
      (setq def (LI:SafeGetFilterDef defs name))
      (if def
        (progn
          (setq raw (LI:ForceString (cadr def)))
          (setq tokens (LI:SafeSplit raw ","))
          (if tokens
            (foreach token tokens
              (setq token (LI:SafeTrim token))
              (if (/= token "")
                (progn
                  (setq ref (LI:SafeGetFilterRef token))
                  (if ref
                    (progn
                      (if (LI:SafeGetFilterDef defs ref)
                        (progn
                          (setq childLayers (LI:SafeResolve ref defs visited))
                          (if childLayers
                            (foreach l childLayers (setq result (LI:SafeAddUnique result l)))
                          )
                        )
                        (princ (strcat "\n[Предупреждение] Фильтр-ссылка не найден: " ref))
                      )
                    )
                    (setq result (LI:SafeAddUnique result token))
                  )
                )
              )
            )
          )
        )
      )
      result
    )
  )
)

;; Фиксированные заголовки для фильтров
(defun LI:FixedFilterHeaderMap ()
  (list (cons "ИМЯ ФИЛЬТРА" 0) (cons "СПИСОК СЛОЕВ" 1) (cons "ВЫРАЖЕНИЕ" 1))
)

;; Поиск строки заголовка
(defun LI:FindFilterHeaderIndex (rows / i row vals found v)
  (setq i 0)
  (foreach row rows
    (if (not found)
      (progn
        (setq vals (LI:GetCellValues row))
        (foreach v vals
          (if (and (not found) (eq (type v) 'STR) (= (strcase (LI:SafeTrim v)) "ИМЯ ФИЛЬТРА"))
            (setq found i)
          )
        )
      )
    )
    (setq i (1+ i))
  )
  found
)

;; Удаление первых n строк
(defun LI:DropRows (lst n / i out)
  (setq i 0 out nil)
  (foreach x lst (if (>= i n) (setq out (append out (list x)))) (setq i (1+ i)))
  out
)

(defun LI:CreateGroupFilterCmd (name layerString / res)
  (setq res
    (vl-catch-all-apply
      'vl-cmdf
      (list
        "._-LAYER"
        "_Filter"
        "_New"
        "_Group"
        ""              ; родительский фильтр - по умолчанию
        layerString     ; список слоёв для включения
        name            ; имя создаваемого фильтра
        "_Yes"          ; если AutoCAD спросит замену существующего фильтра
        "_Exit"         ; выход из подменю фильтров
        ""              ; выход из команды -LAYER
      )
    )
  )

  (not (vl-catch-all-error-p res))
)

(defun LI:SetCurrentFilterAll ( / res)
  (setq res
    (vl-catch-all-apply
      'vl-cmdf
      (list
        "._-LAYER"
        "_Filter"
        "_Set"
        "Все"
        ""
      )
    )
  )

  ;; Если русское "Все" вдруг не сработало, пробуем английское "All"
  (if (vl-catch-all-error-p res)
    (vl-catch-all-apply
      'vl-cmdf
      (list
        "._-LAYER"
        "_Filter"
        "_Set"
        "All"
        ""
      )
    )
  )
)

(defun LI:DeleteFilterCmd (name / res)
  (setq res
    (vl-catch-all-apply
      'vl-cmdf
      (list
        "._-LAYER"
        "_Filter"
        "_Delete"
        name
        ""
      )
    )
  )

  (not (vl-catch-all-error-p res))
)

(defun LI:CreateGroupFilterCmd (name layerString / res)
  (setq res
    (vl-catch-all-apply
      'vl-cmdf
      (list
        "._-LAYER"
        "_Filter"
        "_New"
        "_Group"
        ""              ; родительский фильтр - по умолчанию
        layerString     ; список слоёв
        name            ; имя фильтра
        "_Exit"         ; выход из подменю фильтров
        ""              ; выход из команды -LAYER
      )
    )
  )

  (not (vl-catch-all-error-p res))
)

;;; ============================================================
;;; Основная функция импорта фильтров
;;; ============================================================

(defun LI:ImportFilters (doc xml / sheet rows headerIndex headers dataRows map row vals name layersList filterDefs uniqueDefs def origName rawList resolved layerString res)
  (princ "\nЧтение фильтров слоёв...")
  (setq filterDefs nil)
  (setq sheet (LI:GetWorksheet xml "Фильтры"))
  
  (if (not sheet)
    (princ "\nЛист 'Фильтры' не найден в XML.")
    (progn
      (setq rows (LI:GetRows sheet))
      (princ (strcat "\nНайдено строк на листе 'Фильтры': " (itoa (length rows))))
      
      (if (< (length rows) 2)
        (princ "\nВ файле нет данных о фильтрах.")
        (progn
          (setq headerIndex (LI:FindFilterHeaderIndex rows))
          (if headerIndex
            (progn
              (setq headers (LI:GetCellValues (nth headerIndex rows)))
              (setq dataRows (LI:DropRows rows (1+ headerIndex)))
              (setq map (LI:BuildHeaderMap headers))
              (if (or (not (assoc "ИМЯ ФИЛЬТРА" map)) (not (assoc "СПИСОК СЛОЕВ" map)))
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
            (setq layersList (LI:ForceString (LI:GetByHeader vals map "Список слоев")))
            (if (= layersList "") (setq layersList (LI:ForceString (LI:GetByHeader vals map "Выражение"))))
            
            (if (and (/= name "") (/= (strcase name) "ИМЯ ФИЛЬТРА"))
              (setq filterDefs (append filterDefs (list (list name layersList))))
            )
          )
          
          (setq uniqueDefs nil)
          (foreach def filterDefs
            (if (not (LI:SafeGetFilterDef uniqueDefs (car def)))
              (setq uniqueDefs (append uniqueDefs (list def)))
            )
          )
          (setq filterDefs uniqueDefs)
          
          (princ (strcat "\nНайдено фильтров: " (itoa (length filterDefs))))

          ;; Сбрасываем текущий фильтр на "Все",
          ;; чтобы не мешать удалению/созданию фильтров.
          (LI:SetCurrentFilterAll)
          
          (foreach def filterDefs
            (setq origName (LI:ForceString (car def)))
            (setq rawList (LI:ForceString (cadr def)))
            
            (princ (strcat "\nФильтр: " origName))
            (princ (strcat "  Исходный список: " rawList))
            
            ;; ВРЕМЕННО: используем исходный список без разбора ссылок
            (setq layerString rawList)
            
            (princ (strcat "  Развёрнутый список: " layerString))
            
                (if (/= layerString "")
                  (progn
                    (princ (strcat "\nСоздание фильтра: " origName))

                    ;; Сначала удаляем существующий фильтр, если он есть
                    (LI:DeleteFilterCmd origName)

                    ;; Создаём фильтр заново
                    (if (LI:CreateGroupFilterCmd origName layerString)
                      (princ (strcat "\n[OK] Фильтр создан: " origName))
                      (princ (strcat "\n[Ошибка] Не удалось создать фильтр: " origName))
                    )
                  )
                  (princ (strcat "\n[Пропущено] Фильтр без слоёв: " origName))
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

      ;; Запускаем импорт фильтров
      (LI:ImportFilters doc xml)

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
  ;; Пытаемся найти шаблон автоматически
  (setq initial (LI:FindTemplateFile))

  ;; Если не нашли, берём папку чертежа
  (if (or (not initial) (not (eq (type initial) 'STR)))
    (setq initial (LI:DrawingDir))
  )

  ;; Если папка всё равно не получилась, используем пустую строку,
  ;; чтобы getfiled не получил nil и не выдал ошибку типа аргумента.
  (if (or (not initial) (not (eq (type initial) 'STR)))
    (setq initial "")
  )

  ;; Вызываем диалог выбора файла
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

  ;; Проверяем, что пользователь действительно выбрал файл
  (if (and fname (eq (type fname) 'STR) (/= fname ""))
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