;|
刷在一个图层+
炸开+
统一标高TYBG+
切断相交线overkill+
线变复线TLINETOPOLY +
消除重线XCCX+
pe合并多段线 +
JOIN
|;
(vl-load-com)
(defun c:hb()
 (command"undo""be")
 (xyp-MkLaCo"250"250)
 (setq acadObj(vlax-get-acad-object)doc(vla-get-ActiveDocument acadObj))
 (setq p2 (getcorner(setq p1(getpoint"\n拾取一点")) "\n拾取角点："))
 (vla-ZoomWindow acadObj (vlax-3d-point p1)(vlax-3d-point p2))
 (setq ptstr(strcat(rtos(car p1)2 2)","(rtos(cadr p1)2 2)" "(rtos(car p2)2 2)","(rtos(cadr p2)2 2)))
 ;刷在一个图层
 (command"_change"p1 p2"""p""la""250""")
 ;炸开
 (command"_explode"p1 p2"")
 ; 统一标高
 (command"_change"p1 p2 """p""e"0"")
 ;切断相交线
 (vla-SendCommand doc(strcat"-overkill "ptstr" " " "));;切断相交线overkill
 ;线变复线TLINETOPOLY
 (vla-SendCommand doc(strcat"_TLINETOPOLY "ptstr" " " "))
 ;消除重线
 (command"_TREMOVEDUP" p1 p2 "")
 ;pe合并多段线
 (vla-SendCommand doc(strcat"_pedit M " ptstr "  y j" " 1 " " "))
 (command"undo""end")
)
(defun modent1 (el tylst / c)  ;(modent el '((8 . "1") (62 . 1)))
  (foreach n tylst (if (setq c (assoc (car n) el)) (setq el (subst n c el))(setq el (append el (list n))))) (entmod el))
(defun SstoEs(ss / a en lst)
  (if ss(progn(setq a -1) (while(setq en(ssname ss(setq a(1+ a)))) (setq lst(cons en lst))))) lst)
(defun str2lst (str /)
  (read(vl-list->string
  (apply 'append(mapcar '(lambda (x)(if (= 32 x) (list 34 32 34) (list x)))(append (list 40 34)(vl-string->list str)(list 34 41)))))))
(defun dxf (ent i) (cond ((= (type ent) 'ename) (cdr (assoc i (entget ent '("*")))))((= (type ent) 'list)(cdr (assoc i ent)))))
(defun xyp-MkLaCo (la co /)
 (vla-put-color((if(tblsearch"layer"la)vla-item vla-add)(vla-get-Layers(vla-get-ActiveDocument(vlax-get-acad-object)))la)co)
 (mapcar'setvar'("cecolor""celtype""clayer")(list"ByLayer""ByLayer"la)))
