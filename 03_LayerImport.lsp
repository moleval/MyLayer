;;; ============================================================
;;; 03_LayerImport.lsp
;;; Импорт слоёв и фильтров из Excel-XML (формат 2003)
;;; Универсальное чтение (UTF-8 / Windows-1251 / UTF-16).
;;; Вложенные фильтры: >>Имя = потомок этого фильтра.
;;; Слои потомков автоматически поднимаются в родителя —
;;; иначе AutoCAD создаёт пустые вложенные фильтры.
;;; ============================================================

(vl-load-com)

(setq *LI:ALWAYS-ASK* nil)
(setq *LI:DELETE-EXISTING-FILTERS* nil)
(setq *LI:SET-CURRENT-FILTER* nil)

;;; ============================================================
;;; Базовые безопасные функции
;;; ============================================================

(defun LI:IsString (x) (eq (type x) 'STR))
(defun LI:IsError (x) (if x (vl-catch-all-error-p x) nil))
(defun LI:ForceString (x)
  (cond ((null x) "") ((LI:IsString x) x) (t (vl-prin1-to-string x)))
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
  (setq s (LI:ForceString s) find (LI:ForceString find) rep (LI:ForceString rep))
  (if (or (= s "") (= find "")) s
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
  (if (= s "") nil (progn (setq v (distof s)) (if (numberp v) (fix v) nil)))
)
(defun LI:YesNoTrue (s / tstr)
  (setq tstr (strcase (LI:SafeTrim s)))
  (or (= tstr "ДА") (= tstr "YES") (= tstr "TRUE") (= tstr "1")
      (= tstr "-1") (= tstr "ON") (= tstr "ИСТИНА"))
)

;;; ============================================================
;;; Чтение XML (ADODB.Stream с перебором кодировок)
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
      (if (and txt (eq (type txt) 'STR) (> (strlen txt) 0)) txt nil)
    )
    nil
  )
)

(defun LI:ReadFile (fname / charsets cs txt)
  (setq charsets (list "utf-8" "windows-1251" "unicode"))
  (setq txt nil)
  (foreach cs charsets
    (if (not txt)
      (progn
        (setq txt (LI:ReadFileWithCharset fname cs))
        (if (and txt (not (vl-string-search "<Worksheet" txt)))
          (setq txt nil)
        )
      )
    )
  )
  txt
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
  (setq rows nil pos 0)
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

(defun LI:GetCellValues (row / vals pos tagEnd start0 end val)
  (setq vals nil pos 0)
  (while (setq pos (vl-string-search "<Data" row pos))
    (setq tagEnd (vl-string-search ">" row pos))
    (if tagEnd
      (progn
        (setq start0 (1+ tagEnd))
        (setq end (vl-string-search "</Data>" row start0))
        (if end
          (progn
            (setq val (substr row (1+ start0) (- end start0)))
            (setq vals (append vals (list (LI:XmlUnescape val))))
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
;;; Заголовки и работа со слоями
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
  (setq i 0 map nil)
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

(defun LI:EnsureLayer (doc name / layers res)
  (if (tblsearch "LAYER" name) T
    (progn
      (setq layers (vla-get-Layers doc))
      (setq res (vl-catch-all-apply 'vla-add (list layers name)))
      (not (LI:IsError res))
    )
  )
)

(defun LI:SetDxf (ent code val / old)
  (setq old (assoc code ent))
  (if old (subst (cons code val) old ent) (append ent (list (cons code val))))
)

(defun LI:SetBit (flags bit on)
  (if on (logior flags bit) (if (= (logand flags bit) bit) (- flags bit) flags))
)

(defun LI:LoadLinetype (doc name / ltypes res)
  (if (and name (/= name "") (not (tblsearch "LTYPE" name)))
    (progn
      (setq ltypes (vl-catch-all-apply 'vla-get-Linetypes (list doc)))
      (if (not (LI:IsError ltypes))
        (progn
          (setq res (vl-catch-all-apply 'vlax-invoke (list ltypes 'Load name)))
          (if (LI:IsError res)
            (setq res (vl-catch-all-apply 'vlax-invoke (list ltypes 'Load name "")))
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
              (if (LI:IsError res) (vl-catch-all-apply 'vlax-put (list obj 'Value num)))
            )
          )
        )
      )
    )
  )
)

(defun LI:SetPlotStyle (layer val / res)
  (setq res (vl-catch-all-apply 'vlax-put (list layer 'PlotStyleName val)))
  (if (LI:IsError res) (vl-catch-all-apply 'vlax-put (list layer 'PlotStyle val)))
)

(defun LI:ApplyLayerRow (doc vals map / name isNew ename ent flags aci onStr on oldColor colorVal desc lt lw plotStr frozen frozenVp locked res layerObj transStr plotStyle)
  (setq name (LI:SafeTrim (LI:GetByHeader vals map "Имя слоя")))
  (if (= name "") nil
    (progn
      (setq isNew (not (tblsearch "LAYER" name)))
      (if (not (LI:EnsureLayer doc name))
        (progn (princ (strcat "\n[Ошибка] Не удалось создать слой: " name)) "Ошибка")
        (progn
          (setq ename (tblobjname "LAYER" name))
          (if (not ename)
            (progn (princ (strcat "\n[Ошибка] Не найден объект слоя: " name)) "Ошибка")
            (progn
              (setq ent (entget ename))
              (setq desc (LI:SafeTrim (LI:GetByHeader vals map "Описание")))
              (if (/= desc "") (setq ent (LI:SetDxf ent 3 desc)))
              (setq lt (LI:SafeTrim (LI:GetByHeader vals map "Тип линии")))
              (if (/= lt "")
                (progn
                  (if (not (tblsearch "LTYPE" lt)) (LI:LoadLinetype doc lt))
                  (if (tblsearch "LTYPE" lt)
                    (setq ent (LI:SetDxf ent 6 lt))
                    (if (tblsearch "LTYPE" "Continuous")
                      (progn (setq ent (LI:SetDxf ent 6 "Continuous"))
                        (princ (strcat "\n[Предупреждение] Тип линии не найден: " lt ". Назначен Continuous.")))
                      (princ (strcat "\n[Предупреждение] Тип линии не найден: " lt))
                    )
                  )
                )
              )
              (setq aci (LI:ToInt (LI:GetByHeader vals map "Цвет ACI")))
              (setq onStr (LI:SafeTrim (LI:GetByHeader vals map "Включен")))
              (if (= onStr "") (setq on T) (setq on (LI:YesNoTrue onStr)))
              (if (and aci (>= aci 1) (<= aci 255))
                (progn
                  (setq colorVal (abs aci))
                  (if (not on) (setq colorVal (- colorVal)))
                  (setq ent (LI:SetDxf ent 62 colorVal))
                )
                (progn
                  (setq oldColor (cdr (assoc 62 ent)))
                  (if (and oldColor (/= oldColor 0))
                    (progn
                      (if on (setq colorVal (abs oldColor)) (setq colorVal (- (abs oldColor))))
                      (setq ent (LI:SetDxf ent 62 colorVal))
                    )
                  )
                )
              )
              (setq lw (LI:ToInt (LI:GetByHeader vals map "Вес линии код")))
              (if (numberp lw) (setq ent (LI:SetDxf ent 370 lw)))
              (setq flags (if (cdr (assoc 70 ent)) (cdr (assoc 70 ent)) 0))
              (setq frozen (LI:YesNoTrue (LI:GetByHeader vals map "Заморожен")))
              (setq frozenVp (LI:YesNoTrue (LI:GetByHeader vals map "Заморожен в новых ВЭ")))
              (setq locked (LI:YesNoTrue (LI:GetByHeader vals map "Заблокирован")))
              (setq flags (LI:SetBit flags 1 frozen))
              (setq flags (LI:SetBit flags 2 frozenVp))
              (setq flags (LI:SetBit flags 4 locked))
              (setq ent (LI:SetDxf ent 70 flags))
              (setq plotStr (LI:SafeTrim (LI:GetByHeader vals map "Печатается")))
              (if (/= plotStr "") (setq ent (LI:SetDxf ent 290 (if (LI:YesNoTrue plotStr) 1 0))))
              (setq res (vl-catch-all-apply 'entmod (list ent)))
              (if (LI:IsError res)
                (progn (princ (strcat "\n[Ошибка] Не удалось изменить слой: " name)) "Ошибка")
                (progn
                  (setq layerObj (LI:AsVla ename))
                  (if layerObj
                    (progn
                      (setq transStr (LI:SafeTrim (LI:GetByHeader vals map "Прозрачность")))
                      (if (/= transStr "") (LI:SetTransparency layerObj transStr))
                      (setq plotStyle (LI:SafeTrim (LI:GetByHeader vals map "Стиль печати")))
                      (if (/= plotStyle "") (LI:SetPlotStyle layerObj plotStyle))
                    )
                  )
                  (if isNew (princ (strcat "\n[Создан] " name)) (princ (strcat "\n[Обновлён] " name)))
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

;;; ============================================================
;;; Функции для фильтров
;;; ============================================================

(defun LI:SafeSplit (s delim / tokens pos start lenf)
  (setq s (LI:ForceString s) delim (LI:ForceString delim))
  (if (or (= s "") (= delim "")) nil
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

(defun LI:SafeAddUnique (lst s / found x)
  (setq s (LI:SafeTrim s))
  (if (= s "") lst
    (progn
      (foreach x lst (if (= (strcase (LI:ForceString x)) (strcase s)) (setq found T)))
      (if found lst (append lst (list s)))
    )
  )
)

(defun LI:SafeStrCase (s) (strcase (LI:ForceString s)))

(defun LI:SafeGetFilterDef (defs name / found d)
  (foreach d defs
    (if (and (listp d) (car d) (= (LI:SafeStrCase (car d)) (LI:SafeStrCase name)))
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
  (while (and (> (strlen token) 1) (or (= (substr token 1 1) "\"") (= (substr token 1 1) "'")))
    (setq token (LI:SafeTrim (substr token 2)))
  )
  (while (and (> (strlen token) 1) (or (= (substr token (strlen token) 1) "\"") (= (substr token (strlen token) 1) "'")))
    (setq token (LI:SafeTrim (substr token 1 (1- (strlen token)))))
  )
  (setq up (strcase token))
  (cond
    ((and (setq pos (vl-string-search "@" token)) (<= pos 5)) (LI:SafeTrim (substr token (+ pos 2))))
    ((and (setq pos (vl-string-search ">>" token)) (<= pos 5)) (LI:SafeTrim (substr token (+ pos 3))))
    ((and (setq pos (vl-string-search "FILTER:" up)) (<= pos 5)) (LI:SafeTrim (substr token (+ pos 8))))
    ((and (setq pos (vl-string-search "ФИЛЬТР:" up)) (<= pos 5)) (LI:SafeTrim (substr token (+ pos 8))))
    (t nil)
  )
)

(defun LI:ParseFilterDef (raw / tokens direct children token ref)
  (setq tokens (LI:SafeSplit raw ","))
  (setq direct nil children nil)
  (if tokens
    (foreach token tokens
      (setq token (LI:SafeTrim token))
      (if (/= token "")
        (progn
          (setq ref (vl-catch-all-apply 'LI:SafeGetFilterRef (list token)))
          (if (LI:IsError ref) (setq ref nil))
          (cond
            ((and (LI:IsString ref) (/= ref "")) (setq children (LI:SafeAddUnique children ref)))
            ((null ref) (setq direct (LI:SafeAddUnique direct token)))
            (t (if (not (and (LI:IsString ref) (= ref ""))) (setq direct (LI:SafeAddUnique direct token))))
          )
        )
      )
    )
  )
  (cons direct children)
)

(defun LI:AssocNoCase (alist key / keyUp found pair carVal)
  (setq keyUp (LI:SafeStrCase key))
  (foreach pair alist
    (setq carVal (car pair))
    (if (and carVal (LI:IsString carVal) (= (LI:SafeStrCase carVal) keyUp))
      (setq found pair)
    )
  )
  found
)

(defun LI:FindParsedDef (parsedDefs name / found p)
  (foreach p parsedDefs
    (if (= (LI:SafeStrCase (nth 0 p)) (LI:SafeStrCase name)) (setq found p))
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
          (if (and (not found) (LI:IsString v) (= (strcase (LI:SafeTrim v)) "ИМЯ ФИЛЬТРА"))
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
  (setq i 0 out nil)
  (foreach x lst (if (>= i n) (setq out (append out (list x)))) (setq i (1+ i)))
  out
)

(defun LI:SetCurrentFilterAll ( / )
  (command "._-LAYER" "_Filter" "_Set" "Все" "_Exit" "")
)

(defun LI:DeleteFilterCmd (name)
  (if (and name (/= name "")) (command "._-LAYER" "_Filter" "_Delete" name ""))
  T
)

(defun LI:CreateGroupFilterWithParentCmd (name layerString parentName / parentInput)
  (if (or (not name) (= name "")) nil
    (progn
      (setq parentInput "")
      (if (and parentName (/= parentName "")) (setq parentInput parentName))
      (if (not layerString) (setq layerString ""))
      (command "._-LAYER" "_Filter" "_New" "_Group" parentInput layerString name "_Exit" "")
      T
    )
  )
)

;;; ============================================================
;;; Вложенность: >>Имя = потомок. Родитель наследует слои потомков.
;;; ============================================================

(defun LI:NameInList (lst name / found x)
  (foreach x lst
    (if (= (LI:SafeStrCase x) (LI:SafeStrCase name)) (setq found T))
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

(defun LI:BuildFilterOrder (parsedDefs / remaining ordered createdNames def name parent placed progress)
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

;;; Полный список слоёв фильтра = собственные + все слои потомков.
;;; Нужен, потому что AutoCAD ограничивает слои ребёнка слоями родителя:
;;; если у родителя пусто, у ребёнка тоже будет пусто.
(defun LI:CollectLayersDeep (parsedDefs name visited / p direct children child sub)
  (if (LI:NameInList visited name) nil
    (progn
      (setq p (LI:FindParsedDef parsedDefs name))
      (if (null p) nil
        (progn
          (setq direct (nth 1 p))
          (setq children (nth 2 p))
          (if (not (listp direct)) (setq direct nil))
          (foreach child children
            (setq sub (LI:CollectLayersDeep parsedDefs child (append visited (list name))))
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
;;; Импорт фильтров с вложенностью
;;; ============================================================

(defun LI:ImportFilters (doc xml / sheet rows headerIndex headers dataRows map row vals name layersList filterDefs uniqueDefs def parsedDefs p raw parsed direct children directString fullLayers orderedDefs createdNames parentName)
  (princ "\nЧтение фильтров слоёв...")
  (setq filterDefs nil)
  (setq sheet (LI:GetWorksheet xml "Фильтры"))
  (if (not sheet) (princ "\nЛист 'Фильтры' не найден в XML.")
    (progn
      (setq rows (LI:GetRows sheet))
      (princ (strcat "\nНайдено строк на листе 'Фильтры': " (itoa (length rows))))
      (if (< (length rows) 2) (princ "\nВ файле нет данных о фильтрах.")
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
            (progn (setq dataRows rows) (setq map (LI:FixedFilterHeaderMap)))
          )

          (foreach row dataRows
            (setq vals (LI:GetCellValues row))
            (setq name (LI:SafeTrim (LI:GetByHeader vals map "Имя фильтра")))
            (setq layersList (LI:ForceString (LI:GetByHeader vals map "Список слоев")))
            (if (= layersList "") (setq layersList (LI:ForceString (LI:GetByHeader vals map "Выражение"))))
            (if (and (/= name "") (/= (LI:SafeStrCase name) "ИМЯ ФИЛЬТРА"))
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

          (princ "\nЭтап: разбор фильтров...")
          (setq parsedDefs nil)
          (foreach def filterDefs
            (setq name (LI:ForceString (car def)))
            (setq raw (LI:ForceString (cadr def)))
            (setq parsed (vl-catch-all-apply 'LI:ParseFilterDef (list raw)))
            (if (LI:IsError parsed) (setq parsed (cons nil nil)))
            (if (not (listp parsed)) (setq parsed (cons nil nil)))
            (setq direct (car parsed))
            (setq children (cdr parsed))
            (if (not (listp direct)) (setq direct nil))
            (if (not (listp children)) (setq children nil))
            (setq parsedDefs (append parsedDefs (list (list name direct children))))
          )
          (princ (strcat "\nРазобрано фильтров: " (itoa (length parsedDefs))))

          (princ "\nСтруктура фильтров:")
          (foreach p parsedDefs
            (setq name (nth 0 p))
            (setq direct (nth 1 p))
            (setq children (nth 2 p))
            (setq parentName (LI:FindParentOf parsedDefs name))
            (if parentName
              (princ (strcat "\n  " name "  [родитель: " parentName "]"))
              (princ (strcat "\n  " name "  (корневой)"))
            )
            (if (and (listp children) children)
              (princ (strcat "  потомки: " (LI:SafeListToComma children)))
            )
            (if (and (listp direct) direct)
              (princ (strcat "  слои: " (LI:SafeListToComma direct)))
            )
          )

          (princ "\nЭтап: построение порядка создания (с учётом вложенности)...")
          (setq orderedDefs (LI:BuildFilterOrder parsedDefs))
          (princ (strcat "\nЗапланировано к созданию: " (itoa (length orderedDefs))))

          (princ "\nЭтап: создание фильтров...")
          (setq createdNames nil)
          (foreach p orderedDefs
            (setq name (nth 0 p))
            (setq parentName (LI:FindParentOf parsedDefs name))
            (if (null parentName) (setq parentName ""))

            ;; Собираем ПОЛНЫЙ набор слоёв: собственные + все слои потомков.
            ;; Иначе вложенный фильтр будет пустым (ограничение AutoCAD).
            (setq fullLayers (LI:CollectLayersDeep parsedDefs name nil))
            (setq directString (LI:SafeListToComma fullLayers))

            (if (and (/= parentName "") (not (LI:NameInList createdNames parentName)))
              (progn
                (princ (strcat "\n[Внимание] Родитель '" parentName
                               "' не найден — фильтр '" name "' создаётся как корневой."))
                (setq parentName "")
              )
            )

            (princ (strcat "\nСоздание: " name))
            (if (/= parentName "")
              (princ (strcat "  [внутри: " parentName "]"))
            )
            (princ (strcat "  Слои: " directString))

            (if (LI:CreateGroupFilterWithParentCmd name directString parentName)
              (progn
                (princ " -> OK")
                (setq createdNames (append createdNames (list name)))
              )
              (princ " -> ОШИБКА")
            )
          )

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

(defun LI:ImportFromFile (fname / doc xml sheet rows headers map row vals status created updated errors oldLayer)
  (setq created 0 updated 0 errors 0)
  (setq xml (LI:ReadFile fname))
  (if (not xml) (princ (strcat "\nНе удалось прочитать файл: " fname))
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      (setq oldLayer (getvar "CLAYER"))
      (vl-catch-all-apply 'setvar (list "CLAYER" "0"))
      (setq sheet (LI:GetWorksheet xml "Слои"))
      (if (not sheet) (setq sheet (LI:GetFirstWorksheet xml)))
      (if (not sheet) (princ "\nНе найден лист слоёв в XML-файле.")
        (progn
          (setq rows (LI:GetRows sheet))
          (if (< (length rows) 2) (princ "\nВ файле нет данных слоёв.")
            (progn
              (setq headers (LI:GetCellValues (car rows)))
              (setq map (LI:BuildHeaderMap headers))
              (foreach row (cdr rows)
                (setq vals (LI:GetCellValues row))
                (if vals
                  (progn
                    (setq status (LI:ApplyLayerRow doc vals map))
                    (cond
                      ((= status "Создан") (setq created (1+ created)))
                      ((= status "Обновлён") (setq updated (1+ updated)))
                      ((= status "Ошибка") (setq errors (1+ errors)))
                    )
                  )
                )
              )
            )
          )
        )
      )
      (LI:ImportFilters doc xml)
      (if (and oldLayer (tblsearch "LAYER" oldLayer))
        (vl-catch-all-apply 'setvar (list "CLAYER" oldLayer))
      )
      (princ (strcat "\nГотово.\nСоздано слоёв: " (itoa created) "\nОбновлено слоёв: " (itoa updated) "\nОшибок: " (itoa errors)))
    )
  )
  (princ)
)

;;; ============================================================
;;; Поиск и выбор файла шаблона
;;; ============================================================

(defun LI:EnsureSlash (p)
  (if (and p (/= p "")) (if (= (substr p (strlen p) 1) "\\") p (strcat p "\\")) p)
)

(defun LI:DrawingDir (/ p)
  (setq p (getvar "DWGPREFIX"))
  (if (and p (/= p ""))
    (if (vl-file-directory-p p) (setq p (LI:EnsureSlash p)) (setq p (LI:EnsureSlash (vl-filename-directory p))))
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
          (strcat dir base " Слои.xml")
          (strcat dir base ".xml")
          (strcat dir "Слои и фильтры.xml")
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
  (if (not f) (setq f (findfile "Слои.xml")))
  f
)

(defun LI:SelectTemplateFile (/ initial f)
  (setq initial (LI:FindTemplateFile))
  (if (or (not initial) (not (eq (type initial) 'STR))) (setq initial (LI:DrawingDir)))
  (if (or (not initial) (not (eq (type initial) 'STR))) (setq initial ""))
  (setq f (getfiled "Выберите XML-шаблон слоёв" initial "xml" 0))
  f
)

(defun LI:Run (/ fname)
  (setq fname (LI:FindTemplateFile))
  (if (and fname (not *LI:ALWAYS-ASK*))
    (princ (strcat "\nИспользуется шаблон: " fname))
    (progn
      (if (not fname) (princ "\nШаблон слоёв в папке чертежа не найден."))
      (setq fname (LI:SelectTemplateFile))
    )
  )
  (if (and fname (eq (type fname) 'STR) (/= fname ""))
    (LI:ImportFromFile fname)
    (princ "\nФайл шаблона не выбран.")
  )
  (princ)
)

(defun C:СЛОИЗАГРУЗИТЬ () (LI:Run))
(defun C:LAYERSLOAD () (LI:Run))

;;; ============================================================
;;; ДИАГНОСТИКА XML
;;; ============================================================

(defun C:ДИАГНОСТИКАXML (/ fname cs txt)
  (princ "\n=== ДИАГНОСТИКА XML ===")
  (setq fname (LI:FindTemplateFile))
  (if (not fname)
    (princ "\nШаблон не найден.")
    (progn
      (princ (strcat "\nФайл: " fname))
      (princ (strcat "\nНайден через findfile: " (if (findfile fname) "ДА" "НЕТ")))
      (foreach cs (list "utf-8" "windows-1251" "unicode")
        (setq txt (LI:ReadFileWithCharset fname cs))
        (princ (strcat "\n\nКодировка '" cs "':"))
        (if (not txt)
          (princ " не удалось прочитать или пусто")
          (progn
            (princ (strcat " длина " (itoa (strlen txt)) " симв."))
            (princ (strcat "\n  Первые 60 симв.: ["
                           (substr txt 1 (min 60 (strlen txt))) "]"))
            (princ (strcat "\n  Есть '<Worksheet': "
                           (if (vl-string-search "<Worksheet" txt) "ДА" "нет")))
            (princ (strcat "\n  Есть 'Слои': "
                           (if (vl-string-search "Слои" txt) "ДА" "нет")))
            (princ (strcat "\n  Есть 'Фильтры': "
                           (if (vl-string-search "Фильтры" txt) "ДА" "нет")))
          )
        )
      )
      (setq txt (LI:ReadFile fname))
      (princ "\n\nАвтовыбор LI:ReadFile:")
      (if txt
        (princ (strcat " OK, длина " (itoa (strlen txt)) " симв."))
        (princ " НЕ УДАЛОСЬ прочитать ни в одной кодировке.")
      )
    )
  )
  (princ)
)

;;; ============================================================
;;; Очистка фильтров слоёв
;;; -Layer _Filter _Delete не поддерживает "*", поэтому удаляем
;;; по именам, взятым из XML-шаблона. Делаем 3 прохода, чтобы
;;; дети удалялись раньше родителей.
;;; ============================================================

(defun C:ФИЛЬТРЫОЧИСТИТЬ ( / fname xml sheet rows vals name names pass)
  (princ "\n=== Удаление фильтров слоёв ===")
  (setq names nil)

  ;; 1. Собрать имена фильтров из XML-шаблона
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
                                (/= (LI:SafeStrCase name) "ИМЯ ФИЛЬТРА"))
                      (setq names (LI:SafeAddUnique names name))
                    )
                  )
                )
              )
            )
            (princ "\nЛист 'Фильтры' не найден.")
          )
        )
        (princ "\nНе удалось прочитать XML.")
      )
    )
    (princ "\nШаблон не найден в папке чертежа.")
  )

  ;; 2. Если имён нет — спросить вручную
  (if (null names)
    (progn
      (setq name (getstring T
        "\nИмена фильтров через запятую (Enter — отмена): "))
      (if (and name (/= name ""))
        (setq names (LI:SafeSplit name ","))
      )
    )
  )

  ;; 3. Удалить. Три прохода, чтобы вложенные удалились
  ;;    раньше своих родителей.
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
              ;; После _Delete AutoCAD всегда снова спрашивает
              ;; "Фильтр слоев для удаления [?]:" — как при успехе,
              ;; так и при неудаче. Поэтому:
              ;;   name    - попытка удалить
              ;;   ""      - выход из запроса имени
              ;;   _Exit   - выход из _Filter
              ;;   ""      - выход из -LAYER
              (command "._-LAYER" "_Filter" "_Delete" name "" "_Exit" "")
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

(princ "\n=============================================")
(princ "\nЗагружено: 03_LayerImport.lsp  (>>Имя = потомок, слои наследуются)")
(princ "\nКоманды:")
(princ "\n  СЛОИЗАГРУЗИТЬ")
(princ "\n  LAYERSLOAD")
(princ "\n  ДИАГНОСТИКАXML")
(princ "\n  ФИЛЬТРЫОЧИСТИТЬ   - удалить все фильтры слоёв")
(princ "\n=============================================")
(princ)