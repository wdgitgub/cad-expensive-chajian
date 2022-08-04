

(defun c:gys(/ doc ss obj en subobj color ent sk_lay)
  (vl-load-com)
  (setq sk_lay(sk_getdcl))
  (setq color (acad_colordlg 8))  
  (if(and (or sk_lay color)
          (setq ss(ssget))
          )
    (progn
      (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
      ;(setq sk_lay "0")      
      (while(setq ent(ssname ss 0))
        (setq obj (vlax-ename->vla-object ent))
        (and sk_lay(vla-put-layer (vlax-ename->vla-object ent) sk_lay))
        (and color(vla-put-color (vlax-ename->vla-object ent) Color))        
        (defun sk_block_col(obj /)
          (vlax-for SubObj                    
                    (vla-item (vla-get-blocks doc)
                              (vla-get-name obj) ;获得块名
                              )      ;获取当前文档中的所有块，按照名字从中找到操作者选择的块，返回块中所有对象的集合
            (and sk_lay(vla-put-layer SubObj sk_lay))
            (if (= (vla-get-ObjectName SubObj) "AcDbBlockReference")              
              (sk_block_col SubObj)
              (progn                
                (if (= (vla-get-ObjectName SubObj) "AcDbAttributeDefinition")
                  (sk_att_lay_col ENT (vla-get-TagString  SubObj) sk_lay  Color)
                  (and Color(vla-put-color SubObj Color))
                  )                
              )
              ) ;obj依次为块内每一个图元的对象
            )   ;调用自己，递归，遍历引用中的每层引用
          )
        (if (= (vla-get-ObjectName obj) "AcDbBlockReference") (progn (sk_block_col obj)))        
        (setq ss (ssdel ent ss))
      )
      (vla-regen doc 1)
      (vlax-release-object obj)
      (vlax-release-object doc)
    )  
  )  
  (princ)
  )
(defun sk_getdcl(/ lay_lst sk_lay dcl f s sk_lay_index DCL_ID )
  (vlax-map-collection (vla-get-Layers (vla-get-activedocument (vlax-get-acad-object))) '(lambda (x) (setq lay_lst (cons (vla-get-name x) lay_lst))))
  (setq lay_lst (reverse(mapcar 'vl-princ-to-string lay_lst)))
  (setq DCL (vl-filename-mktemp nil nil ".Lsp"))
        (setq f (open dcl "w"))
        (foreach s '("ch_block_color:dialog {"
                     "    label = \"参数设置\" ;"
                     "    :boxed_row {"
                     "        label = \"设置\" ;"
                     "        :list_box {"
                     "            fixed_height = true ;"
                     "            fixed_width = true ;"
                     "            height = 24 ;"
                     "            label = \"图层选择\" ;"
                     "            width = 30 ;"
                     "            key = sk_lay ;"
                     "       }"
                     "    }"
                     "ok_cancel;"
                     "}"
                    )
          (write-line s f)
        )
        (close f)
  (setq DCL_ID (load_dialog DCL))
  (vl-file-delete dcl)
   (new_dialog "ch_block_color" DCL_ID)
   (start_list "sk_lay")
   (mapcar 'add_list lay_lst)   
   (end_list)
  (action_tile "accept" "(setq sk_lay_index(get_tile \"sk_lay\"))(done_dialog 1) ")
  (action_tile "cancel" "(done_dialog)")
  (start_dialog )
  (unload_dialog DCL_ID)  
  (if sk_lay_index
    (setq sk_lay (nth (atoi sk_lay_index) lay_lst)) nil)
  sk_lay
  )

;;;日期：zml84 于 2010-05-08                                        *
;;;日期：modfiy by edata@2014-6-12                                  *
;;;add layer
(defun sk_att_lay_col (EN ATTNAME sk_lay Color / RETURN E TEST ENT)
  (setq        E EN
        RETURN NIL
        TEST t
  )
  (while (and TEST (setq E (entnext E)))
    (setq ENT (entget E))
    (cond ((not (= (cdr (assoc 0 ENT)) "ATTRIB")) (setq TEST NIL))
          ((= "SEQEND" (cdr (assoc 0 ENT))) (setq TEST NIL))
          ((= (cdr (assoc 2 ENT)) ATTNAME)
           (and sk_lay(setq ENT (subst(cons 8 sk_lay)(assoc 8 ENT) ENT)))
            (if (and Color(assoc 62 ENT))
             (setq ENT (subst(cons 62 Color)(assoc 62 ENT) ENT))
             (setq ENT (cons (cons 62 Color) ENT))
           )
           (entmod ENT)
           (entupd EN)
           (setq RETURN t)
          )
    ) 
  )
  RETURN
  )