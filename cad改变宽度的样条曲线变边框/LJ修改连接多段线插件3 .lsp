;|
ˢ��һ��ͼ��+
ը��+
ͳһ���TYBG+
�ж��ཻ��overkill+
�߱临��TLINETOPOLY +
��������XCCX+
pe�ϲ������ +
JOIN
|;
(vl-load-com)
(defun c:lj()
 (command"undo""be")
;;; (xyp-MkLaCo"250"250)
 (setq acadObj(vlax-get-acad-object)doc(vla-get-ActiveDocument acadObj))
 (setq ss(ssget) ptstr"")
;;; (setq p2 (getcorner(setq p1(getpoint"\nʰȡһ��")) "\nʰȡ�ǵ㣺"))
;;; (vla-ZoomWindow acadObj (vlax-3d-point p1)(vlax-3d-point p2))
;;; (setq ptstr(strcat(rtos(car p1)2 2)","(rtos(cadr p1)2 2)" "(rtos(car p2)2 2)","(rtos(cadr p2)2 2)))
 (vl-cmdf"zoom" "o" ss "")
  
;;; (command"_change" ss """p""la""250""") ;ˢ��һ��ͼ��
  
 (command"_change" ss """p""e"0"")  ; ͳһ���
  
;;; (command"_explode"p1 p2"")  ;ը��
  
;;; (sssetfirst nil ss)
;;; (vla-SendCommand doc(strcat"-overkill "ptstr " " "  "))    ;;�ж��ཻ��overkill
  
;;; (sssetfirst nil ss)
;;; (vla-SendCommand doc(strcat"_TLINETOPOLY "" "" " " ")) ;;�߱临��TLINETOPOLY
  
;;; (sssetfirst nil ss)
;;; (command"_TREMOVEDUP" "") ;��������
 (setq flag t)
 (foreach y(mapcar'(lambda(x)(="LWPOLYLINE"(dxf x 0)))(sstoes ss))(if (not y)(setq flag nil)))
 (if flag(command "_pedit" "m" ss "" "j" 10 "")
         (command "_pedit" "m" ss "" "y" "j" 10 "")
 )
  
;;; (vla-SendCommand doc(strcat"_pedit M " ptstr "  y j" " 1 " " ")) ;pe�ϲ������
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
(defun ss-enlst(ss / enlst)
 (cond
  ((= (type ss) 'PICKSET) (vl-remove-if-not '(lambda (x) (= (type x) 'ENAME)) (mapcar 'cadr (ssnamex SS)))  )
  ((= (type ss) 'LIST)    (setq enlst (ssadd))  (last (mapcar '(lambda (x) (ssadd x enlst)) ss))  )
))