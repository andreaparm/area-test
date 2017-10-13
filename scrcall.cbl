       identification division.
       program-id.    scrcall.
       Author.        Andrea Parmeggiani - Eurosystem.
      *
      **------------------------------------------------------**
      ** Generazione maschere 
      **------------------------------------------------------**
      ** Programma generalizzato di innesco
      **------------------------------------------------------**
      *
       environment division.
       configuration section.
       source-computer. acu-cobol.
       object-computer. acu-cobol.
       special-names.
           decimal-point is comma.
       input-output section.
       file-control.
      *
       data division.
       file section.
      *
       working-storage section.
      *kostanti
       copy "costanti.cpy".
       78  k-program-id               value "CRMPAR".

      *77 ext-scr-dir                   pic x(70) is external.
       77 ext-out-dir                   pic x(70) is external.
       77 w-current-dir                 pic x(70).
       77 w-9-1                         pic 9(01).
       77 i                             pic 9(04).
       77 j                             pic 9(04).
       77 z                             pic 9(04).
       77 k                             pic 9(04).
       77 l                             pic 9(04).
       77 m                             pic 9(04).
       77 r                             pic 9(04).
       77 zeta2                         pic zz.
       77 zeta4                         pic zzzz.
       77 w-link                        pic x(1000).
       77 w-status                      pic 9(09) comp-4.

       77  par-ope                      pic x(03).
       77  par-post                     pic x(08).
       77  par-prog                     pic x(08).
      *
       01 datasis                  pic 9(08).      
       01 orasis                   pic 9(08).      
       01 orasis-r redefines orasis.
         02 orasis-oramin          pic 9(04).
         02 filler                 pic 9(04).
      *
       copy "utilsenv.cpy".
       copy "wgrave.cpy".
       copy "wnscr.cpy".
       copy "wopenf.cpy".
       copy "wstato.cpy".
       copy "cogazien.cpy".
      *
       copy "wcont.cpy".
       copy "wcont1.cpy".

      *
       procedure division chaining par-ope par-post par-prog.
      *
       main section.
       apri.
           move k-program-id             to prog-err w-nome-hlp
           perform z-99-init-program

           move "S"                to ext-ambiente-gui
           
           move par-ope            to wo-oper ext-oper.
           move par-post           to ext-postazione

           accept ext-scr-dir	 from environment "SCR-DIR"
           if ext-scr-dir = spaces 
              move "Variabile 'SCR-DIR' non definita; forzo 'C:\ECOGE\SC
      -         "R'..."	           to wb-msg
              perform vbx-msg
              move "C:\ECOGE\SCR"  to ext-scr-dir
           end-if

           initialize util-senv
           move "S"                to senv-run-type
           call "SETENV"        using stringhe util-senv
           cancel "SETENV"

      **----------------------------------------------------------**
      ** Imposto il direttorio di output
      **----------------------------------------------------------**
           call "C$CHDIR"       using w-current-dir, w-status
           move spaces             to ext-out-dir
           string w-current-dir       delimited "  "
              ext-os-slash "screens"  delimited size into ext-out-dir

           perform rd-azi

           if par-ope  = spaces or 
              par-post = spaces or
              par-prog = spaces
              string "Parametri insufficienti; necessari:" k-newline
                 "- Operatore"  k-newline
                 "- Postazione" k-newline
                 "- Programma"  delimited size into wb-msg
              perform vbx-msg-info
              go to fine
           end-if

           accept datasis              from century-date
           accept orasis               from time

           call par-prog                using stringhe w-link
           cancel par-prog
           .
      *
       fine.
       z-chiudi.
           close window w-sv-cur-prg
           perform z-99-exit-program
           goback.
      *
       copy "stato.cpy".
       copy "stato1.cpy".
       copy "winmsg.cpy".
       copy "grave.cpy".
       copy "util1.cpy".
       copy "mmsubw.cpy".
      *
       end program.
       

