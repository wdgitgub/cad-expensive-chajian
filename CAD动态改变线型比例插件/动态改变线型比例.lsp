

(defun C:test ( / dcl_id dclcontent dclname userclick temp)
  (vl-load-com)
  (setq temp (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (command "undo" "be")
  (setq dclcontent (list 
           "qjchenedynamicltscale:dialog{"
           "label=\"线型比例修改 \";"
	   ":button{" 
		"key = \"button1\";"
		"label = \"单独线型比例修改\";}"
	   ":button{" 
		"key = \"button2\";"
		"label = \"全局线型比例修改\";}"
           "ok_cancel;}")
  dclname "qjchendltscale"
  )
  (setq dcl_id (load_dialog (qjchencreatdcl dclname dclcontent))) 
  (if (not (new_dialog "qjchenedynamicltscale" dcl_id)) (exit))
  (action_tile "button1" "(done_dialog 3)")
  (action_tile "button2" "(done_dialog 4)")
  (setq userclick (start_dialog))
  (unload_dialog dcl_id)
  (cond ((= 3 userclick)(qjchenedltscale 1))
        ((= 4 userclick)(qjchenedltscale 2))
  )
  (command "undo" "e")
  (setvar "cmdecho" temp)
)

(defun qjchenedltscale(n / a b gr linetype newscale o orilst overallltscale zq)
  (prompt "\n 请选择某一非continuous线型的物体:")
  (setq a (car (entsel)) o (vlax-ename->vla-object a))
  (setq orilst (vlax-get-property o 'LinetypeScale))
  (setq linetype (cdr (assoc 6 (entget a))))
  (if (= linetype nil) 
      (setq linetype (cdr (assoc 6 (tblsearch "layer" (cdr (assoc 8 (entget a)))))))
  )
  (if (and linetype (/= linetype "Continuous"))
    (progn  
      (setq zq (cdr (assoc 40 (tblsearch "ltype" linetype))))
      (setq overallltscale (getvar "LTSCALE"))
      (setq b (getpoint "\n请选择一个点:"))
      (while (= (car (setq gr (grread nil 5 0))) 5)
	(redraw)
	(grdraw (cadr gr) b 1 1)
	(setq newscale (/ (distance (cadr gr) b) zq overallltscale))
	(apply-props o (list (list "LinetypeScale" newscale)))
      )
      (if (= n 2)
	(progn
	  (setvar "ltscale" (* overallltscale (/ newscale orilst)))
	  (apply-props o (list (list "LinetypeScale" orilst)))
	  (command "regen")
	)
      )
    )
  )
  (vlax-release-object o)
  (princ)
)

(defun apply-props (object proplist)
  (foreach prop proplist
    (if (vlax-property-available-p object (car prop))
      (vlax-put-property object (car prop) (cadr prop))
    )
  )
)

(defun qjchencreatdcl(dclname lst)
 (setq dcl_name (strcat (getenv "temp") "\\" dclname ".dcl")
	f (OPEN dcl_name "w")
  )
  (foreach x lst
     (write-line x f)
   )
  (close f)
  dcl_name
)

;;end main program


(princ "\n本程序采摘自www.lisp123.com更多内容敬请期待！")
(princ "\n本程序命令：test")
(princ)