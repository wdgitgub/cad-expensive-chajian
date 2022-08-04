(defun c:pp()
  (setq ss(sstoes(ssget'((0 . "LWPOLYLINE")))))
  (initget 1 "Yes No")
  (setq del (getkword "是否删除原对象?[是(Y)/否(N)]："))
  (mapcar'(lambda(ex)
    (setq fuzz(dxf ex 43))
    (vla-offset (E2O ex) (/ fuzz 2))
    (setq plst(reverse(plinexy(setq e1(Entlast)))))
    (entdel e1)
    (xg:pline plst nil)
    (vla-offset (E2O ex) (/ fuzz -2))
    (setq plst(reverse(plinexy(setq e1(Entlast)))))
    (entdel e1)
    (xg:pline plst nil)
  )ss)
  (if (= del "Yes")(mapcar'(lambda(x)(entdel x))ss))
  (princ)
)
(defun c:pol()  
  (mapcar'(lambda(ex)
    (setq xk(dxf ex 43) pt1(append(x2plst ex (/ xk 2)) (reverse(x2plst ex (/ xk 2)))))
    (xg:pline (mapcar'(lambda(x)(append x'(0.0)))(vl-remove nil pt1)) t)
  )(sstoes(ssget'((0 . "LWPOLYLINE")))))
)
(defun xg:pline (lst B / PT)
  (entmakeX(append(list '(0 . "LWPOLYLINE")'(100 . "AcDbEntity")'(100 . "AcDbPolyline")(cons 90 (length lst))(IF B '(70 . 129) '(70 . 0)))
      (mapcar '(lambda (pt) (cons 10 pt)) lst))))
(defun plinexy(e / a q m p);;;LWPolyline,POLYLINE顶点,去掉完全重合点
    (setq a(vlax-ename->vla-object e)
  q(vlax-safearray->list(vlax-variant-value(vla-get-Coordinates a)))
    m(vla-get-objectname a)a 0
    m(if(= m"AcDb3dPolyline")3 2))
    (repeat(/(length q)m)
      (cond((= m 2)(setq p1(list(nth a q)(nth(+ a 1)q))))
     ((= m 3)(setq p1(list(nth a q)(nth(+ a 1)q)(nth(+ a 2)q)))))
      (setq p(if (member p1 p)p (append p(list p1)))
      a(+ a m)))
    p)
(defun E2O(Ent) (vlax-ename->vla-object Ent))
(defun O2E(obj) (vlax-vla-object->ename obj))
(defun SstoEs(ss / a en lst)
  (if ss(progn(setq a -1) (while(setq en(ssname ss(setq a(1+ a)))) (setq lst(cons en lst))))) lst)
(defun dxf (ent i) (cond ((= (type ent) 'ename) (cdr (assoc i (entget ent '("*")))))((= (type ent) 'list)(cdr (assoc i ent)))))
(defun x2plst(ent fuzz)
    (vla-offset (E2O ent) fuzz)(setq plst(reverse(plinexy(setq e1(Entlast))))) (entdel e1)
    (vla-offset (E2O ent) (- fuzz))(setq plst(append(plinexy(setq e2(Entlast))) plst)) (entdel e2)
   plst
)
 