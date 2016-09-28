;;-------------------------=={  Print to PDF  }==------------------------;;
;; � 2015 Sergey Berman, berman.sergey@gmail.com
(defun c:ppdf ()
; ��������� ���������
(setq f "ppdf.ini") ;-Opens the file to read
(if (and (setq f (findfile f)) (setq f (open f "r")))
	(progn
	(setq sheatprint (setq dataline (read-line f)))             ;-Reads the header
	(setq setprinter (setq dataline (read-line f)))             ;-Reads the 2st data line
	(setq granicy (setq dataline (read-line f)))                ;-Reads the 3st data line
	(setq mashtab (setq dataline (read-line f)))                ;-Reads the 3st data line
	(setq centrotstup (setq dataline (read-line f)))             ;-Reads the 5st data line
	(setq ctbstyle (setq dataline (read-line f)))                ;-Reads the 6st data line
	(setq setprintsave (setq dataline (read-line f)))            ;-Reads the 7st data line
	(setq pechat (setq dataline (read-line f)))                  ;-Reads the 8st data line	
	(close f)
	)
	(progn
	(print "Error: �� ������ .ini ���� ��������") 
	(abort)
	)
)
; �������� ������ �������
(cond ((= sheatprint "Active") (setq sheatprint (getvar 'TILEMODE)))
      ((= sheatprint "Model") (setvar 'TILEMODE 1)) ; Model
	  ((= sheatprint "Layout") (setvar 'TILEMODE 0)) ; Layout
	  (t (print "Error: ����� �������� �������� 1-� ������ .ini �����. ����������� Model/Layout/Active") (abort))
)
; ������� ������� �������
(SetVar "treedepth" (GETvar "treedepth"))
	(setq bt (getvar "extmin")
      tp (getvar "extmax")
    ) ;_  end setq
	(setq shir (- (car tp) (car bt)))
	(setq dlin (- (cadr tp) (cadr bt)))
; ��������� ������ ������
(setq gipt (sqrt (+ (* shir shir) (* dlin dlin)))) ;������� ���������� :)
(setq pogr 15)
(cond ((and (> gipt (- 364 pogr)) (< gipt (+ 364 pogr))) (setq formatka "A4") (setq colform 1))
      ((and (> gipt (- 515 pogr)) (< gipt (+ 515 pogr))) (setq formatka "A3") (setq colform 2))
      ((and (> gipt (- 697 pogr)) (< gipt (+ 697 pogr))) (setq formatka "A4x3") (setq colform 3))
      ((and (> gipt (- 728 pogr)) (< gipt (+ 728 pogr))) (setq formatka "A2") (setq colform 4))
	  ((and (> gipt (- 892 pogr)) (< gipt (+ 892 pogr))) (setq formatka "A4x4") (setq colform 4))
	  ((and (> gipt (- 986 pogr)) (< gipt (+ 986 pogr))) (setq formatka "A3x3") (setq colform 6))
	  ((and (> gipt (- 1030 pogr)) (< gipt (+ 1030 pogr))) (setq formatka "A1") (setq colform 8))
	  ((and (> gipt (- 1093 pogr)) (< gipt (+ 1093 pogr))) (setq formatka "A4x5") (setq colform 5))
	  ((and (> gipt (- 1261 pogr)) (< gipt (+ 1261 pogr))) (setq formatka "A3x4") (setq colform 8))
	  ((and (> gipt (- 1296 pogr)) (< gipt (+ 1296 pogr))) (setq formatka "A4x6") (setq colform 6))
	  ((and (> gipt (- 1394 pogr)) (< gipt (+ 1394 pogr))) (setq formatka "A2x3") (setq colform 12))
	  ((and (> gipt (- 1501 pogr)) (< gipt (+ 1501 pogr))) (setq formatka "A4x7") (setq colform 7))
	  ((and (> gipt (- 1545 pogr)) (< gipt (+ 1545 pogr))) (setq formatka "A3x5") (setq colform 10))
	  ((and (> gipt (- 1709 pogr)) (< gipt (+ 1709 pogr))) (setq formatka "A4x8") (setq colform 8))
	  ((and (> gipt (- 1784 pogr)) (< gipt (+ 1784 pogr))) (setq formatka "A2x4") (setq colform 16))
	  ((and (> gipt (- 1832 pogr)) (< gipt (+ 1832 pogr))) (setq formatka "A3x6") (setq colform 12))
	  ((and (> gipt (- 1916 pogr)) (< gipt (+ 1916 pogr))) (setq formatka "A4x9") (setq colform 9))
	  ((and (> gipt (- 2122 pogr)) (< gipt (+ 2122 pogr))) (setq formatka "A3x7") (setq colform 14))
	  ((and (> gipt (- 2184 pogr)) (< gipt (+ 2184 pogr))) (setq formatka "A2x5") (setq colform 20))
	  (t (princ "Error: ������� ������ ����������� ��� ��������� ������� ������") (abort))
) ;_ cond

; ������� ��������� ������
(if ( > shir dlin) (setq orientat "���������") (setq orientat "�������"))
(print sheatprint)
	  (COMMAND 	"-������"
			"��"
			(vla-get-name (vla-get-activelayout (vla-get-activedocument (vlax-get-acad-object)))) ;sheatprint
			setprinter
			formatka
			"����������"
			orientat
			"���"
			granicy
			mashtab
			centrotstup
			"��"
			ctbstyle
			"��"
			; ����� ��� ��� ������ � �����
			(if (= (getvar 'TILEMODE) 1) "�������" "���")
			(if (= (getvar 'TILEMODE) 1) (strcat (getvar "dwgprefix") (vl-filename-base (getvar "dwgname")) ".pdf") "���")
			(if (= (getvar 'TILEMODE) 1) setprintsave "���")
			(if (= (getvar 'TILEMODE) 1) pechat (strcat (getvar "dwgprefix") (vl-filename-base (getvar "dwgname")) ".pdf"))
			(if (/= (getvar 'TILEMODE) 1) setprintsave)
			(if (/= (getvar 'TILEMODE) 1) pechat)  
		)
; ������������ ������
(setq fn (strcat (getvar "dwgprefix") "ppdfReport_" (_FormatDate "YYYYMODD") ".csv"))	
; �������� ������������� �����
   (setq fnfind (findfile fn))
; ���� ���� �� ���������� 
   (if (= fnfind nil)
      (progn
		(setq fo (open fn "a"))
		(write-line
          (strcat "����� �����������:;"
            (_FormatDate "DD.MO.YYYY HH:MM")
          )
          fo
        )
        (write-line "" fo)
        (write-line (strcat "����;" "������;" "���-�� �4;") fo)
		(close fo)
	  )
	)
	; ���� ���� ����������
		(setq fo (open fn "a"))
        (write-line 
			(strcat 
				(vl-filename-base (getvar "dwgname")) 
				";" formatka ";" (itoa colform)
			) 
			fo
		)
        (close fo)

    (princ)
)
(princ)

;;-------------------------------------------------------------------------------;;
  (defun _FormatDate ( format )
    (menucmd (strcat "m=$(edtime,$(getvar,DATE)," format ")"))
  )
;;-------------------------------------------------------------------------------;;
(vl-load-com)
(princ)
(princ "\n:: ppdf.lsp | Version 1.1 | � 2015 Sergey Berman, berman.sergey@gmail.com ::")
(princ "\n::��� ����������� ����� �������� ���������� �������� ����� ��� ������������ � AutoCAD")
(princ "(������/���������.../�����/���� ������� � ��������������� ������) - ����������� ���� ���")
(princ "\n:: ���� ppdf.ini �������� ��������� ��������� ������: ::")
(princ "\n:: ������ 1 - ��� ������� ������ (Model/Layout/Active) ::")
(princ "\n:: ������ 2 - ������� ��� ������ (DWG To PDF.pc3) ::")
(princ "\n:: ������ 3 - ��� �������� (�����/�������/������/���/�����/����) ::")
(princ "\n:: ������ 4 - ������� ������ �������� (1=1) ::")
(princ "\n:: ������ 5 - ������������ ��� ������ ������ ������ (������������/1.5,-1.5) ::")
(princ "\n:: ������ 6 - ����� ������ (monochrome.ctb) ::")
(princ "\n:: ������ 7 - ��������� ��������� ���������� ����� (��/���) ::")
(princ "\n:: ������ 8 - ������� � ������ (��/���) ::")
(princ "\n:: �������� \"ppdf\" ��� ������� ������ � PDF ::")
(princ)
;;-------------------------------------------------------------------------------;;
;;                                  End of File                                  ;;
;;-------------------------------------------------------------------------------;;