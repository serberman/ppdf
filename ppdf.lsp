;;-------------------------=={  Print to PDF  }==------------------------;;
;; © 2015 Sergey Berman, berman.sergey@gmail.com
(defun c:ppdf ()
; считываем настройки
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
	(print "Error: Не найден .ini файл настроек") 
	(abort)
	)
)
; выбираем нужную вкладку
(cond ((= sheatprint "Active") (setq sheatprint (getvar 'TILEMODE)))
      ((= sheatprint "Model") (setvar 'TILEMODE 1)) ; Model
	  ((= sheatprint "Layout") (setvar 'TILEMODE 0)) ; Layout
	  (t (print "Error: задан неверный параметр 1-й строки .ini файла. Допускается Model/Layout/Active") (abort))
)
; находим границы чертежа
(SetVar "treedepth" (GETvar "treedepth"))
	(setq bt (getvar "extmin")
      tp (getvar "extmax")
    ) ;_  end setq
	(setq shir (- (car tp) (car bt)))
	(setq dlin (- (cadr tp) (cadr bt)))
; определим нужный формат
(setq gipt (sqrt (+ (* shir shir) (* dlin dlin)))) ;находим гипотенузу :)
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
	  (t (princ "Error: Границы печати превосходят все имеющиеся форматы листов") (abort))
) ;_ cond

; зададим параметры печати
(if ( > shir dlin) (setq orientat "Альбомная") (setq orientat "Книжная"))
(print sheatprint)
	  (COMMAND 	"-Печать"
			"Да"
			(vla-get-name (vla-get-activelayout (vla-get-activedocument (vlax-get-acad-object)))) ;sheatprint
			setprinter
			formatka
			"Миллиметры"
			orientat
			"Нет"
			granicy
			mashtab
			centrotstup
			"Да"
			ctbstyle
			"Да"
			; делим код для модели и листа
			(if (= (getvar 'TILEMODE) 1) "Обычный" "Нет")
			(if (= (getvar 'TILEMODE) 1) (strcat (getvar "dwgprefix") (vl-filename-base (getvar "dwgname")) ".pdf") "Нет")
			(if (= (getvar 'TILEMODE) 1) setprintsave "Нет")
			(if (= (getvar 'TILEMODE) 1) pechat (strcat (getvar "dwgprefix") (vl-filename-base (getvar "dwgname")) ".pdf"))
			(if (/= (getvar 'TILEMODE) 1) setprintsave)
			(if (/= (getvar 'TILEMODE) 1) pechat)  
		)
; формирование отчёта
(setq fn (strcat (getvar "dwgprefix") "ppdfReport_" (_FormatDate "YYYYMODD") ".csv"))	
; проверка существования файла
   (setq fnfind (findfile fn))
; если файл не существует 
   (if (= fnfind nil)
      (progn
		(setq fo (open fn "a"))
		(write-line
          (strcat "Отчёт сформирован:;"
            (_FormatDate "DD.MO.YYYY HH:MM")
          )
          fo
        )
        (write-line "" fo)
        (write-line (strcat "Лист;" "Формат;" "Кол-во А4;") fo)
		(close fo)
	  )
	)
	; Если файл существует
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
(princ "\n:: ppdf.lsp | Version 1.1 | © 2015 Sergey Berman, berman.sergey@gmail.com ::")
(princ "\n::Для подключения файла настроек необходимо добавить адрес его расположения в AutoCAD")
(princ "(Сервис/Настройка.../Файлы/Путь доступа к вспомогательным файлам) - выполняется один раз")
(princ "\n:: Файл ppdf.ini содержит следующие настройки печати: ::")
(princ "\n:: Строка 1 - Тип вкладки печати (Model/Layout/Active) ::")
(princ "\n:: Строка 2 - Плоттер для печати (DWG To PDF.pc3) ::")
(princ "\n:: Строка 3 - Что печатать (Экран/Границы/Лимиты/Вид/Рамка/Лист) ::")
(princ "\n:: Строка 4 - Масштаб печати форматок (1=1) ::")
(princ "\n:: Строка 5 - Центрировать или задать отступ печати (Центрировать/1.5,-1.5) ::")
(princ "\n:: Строка 6 - Стиль печати (monochrome.ctb) ::")
(princ "\n:: Строка 7 - Сохранить изменения параметров листа (Да/Нет) ::")
(princ "\n:: Строка 8 - Перейти к печати (Да/Нет) ::")
(princ "\n:: Наберите \"ppdf\" для запуска печати в PDF ::")
(princ)
;;-------------------------------------------------------------------------------;;
;;                                  End of File                                  ;;
;;-------------------------------------------------------------------------------;;