        identification division.
       program-id.    scrfld.
       Author.        Andrea Parmeggiani - Eurosystem.
      *
      **------------------------------------------------------**
      ** Gestione screens
      **------------------------------------------------------**
      ** Definizione screens
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
       copy "scrforms.fd".
       copy "scrfmpag.fd".
       copy "scrtempl.fd".
       copy "scrfield.fd".
       copy "scrfldvf.fd".
       copy "scrpgmsg.fd".
       copy "scrfldex.fd".
       copy "scrfiles.fd".
       copy "scrpgfil.fd".

       copy "relpacch.fd".
       copy "relprog.fd".
       copy "reldirpa.fd".
      *
       data division.
       file section.
      *
      $xfd file=scrforms
       fd  scrforms
           label record standard.
           copy "scrforms.cpy".
      $xfd file=scrfmpag
       fd  scrfmpag
           label record standard.
           copy "scrfmpag.cpy".
      $xfd file=scrtempl
       fd  scrtempl
           label record standard.
           copy "scrtempl.cpy".
      $xfd file=scrfield
       fd  scrfield
           label record standard.
           copy "scrfield.cpy".
      $xfd file=scrfldvf
       fd  scrfldvf
           label record standard.
           copy "scrfldvf.cpy".
      $xfd file=scrpgmsg
       fd  scrpgmsg
           label record standard.
           copy "scrpgmsg.cpy".
      $xfd file=scrfldex
       fd  scrfldex
           label record standard.
           copy "scrfldex.cpy".
      $xfd file=scrfiles
       fd  scrfiles
           label record standard.
           copy "scrfiles.cpy".
      $xfd file=scrpgfil
       fd  scrpgfil
           label record standard.
           copy "scrpgfil.cpy".

       fd  relpacch
           label record standard.
           copy "relpacch.cpy".
       fd  relprog
           label record standard.
           copy "relprog.cpy".
       fd  reldirpa
           label record standard.
           copy "reldirpa.cpy".
      *
       working-storage section.
       copy "scrfld.wrk".
       copy "costanti-screen.cpy".
      *******************************************************************
      * Numero massimo di elementi della griglia, supponendo che sia    *
      * massimizzata                                                    *
      *******************************************************************
       78 k-max-ele-tab               value 200.

      **-------------------------------------------------------------**
      ** ID pulsanti custom
      **-------------------------------------------------------------**
       78  k-f2-note-campo            value 1.
       78  k-f6-file                  value 2.
       78  k-f7-trasporta             value 3.
       78  k-f9-messaggi              value 4.
       78  k-f10-gridsist             value 5.
       78  k-f11-msg-stampa           value 6.
       78  k-f4-importa               value 7.
      *
      *---------------------------------------------------------------**
      * Voci del menu principale
      *---------------------------------------------------------------**
       78 k-menu-id-rigenera-tutto    value 200.
       78  k-menu-id-rilascio-spot    value 206.
       78  k-menu-id-generazione-nav  value 207.
       78  k-menu-id-navigazione      value 208.
       78  k-menu-id-azzera-id-assoluto value 209.
      *-------------------------------------------------------------**
      * Eventi collegati al menu 'lingua'
      *-------------------------------------------------------------**
       78  k-menu-id-esportazione     value 201.
       78  k-menu-id-importa-testi    value 202.
       78  k-menu-id-tabella-lingue   value 203.
       78  k-menu-id-testi-generali   value 204.
       78  k-menu-id-testi-programma  value 205.
      *-------------------------------------------------------------**
      * Eventi collegati al menu 'DB'
      *-------------------------------------------------------------**
       78  k-menu-id-tabelle          value 210.
       78  k-menu-id-template         value 211.
       78  k-menu-id-dipendenze       value 212.

      **------------------------------------------------------------**
      ** Handles per la gestione di un menu a tendina
      **------------------------------------------------------------**
       01  tab-handle-menu.
         02 thm-main            handle of menu.
         02 thm-file            handle of menu.
         02 thm-lingua          handle of menu.
         02 thm-db              handle of menu.
         02 thm-help            handle of menu.
       77 myResult                      pic s9(09).

       01  tab-alfabeto pic x(26) value
           "abcdefghijklmnopqrstuvwxyz".
       01  tab-alfabeto-r redefines tab-alfabeto.
         02 talf-ele occurs 26.
           03 talf-lettera      pic x(01).

       78  k-versione-ges-tabella        value 7.


      *******************************************************************
      * Identifica l'operazione da eseguire quando sono sulla riga di   *
      * dettaglio: (I)nserimento, (M)odifica                            *
      *******************************************************************
       77 a-operazione                  pic x(01).
      **---------------------------------------------------------------**
      ** Identifico se appendere una riga (A) o inserirla alla posizione
      **   corrente (I)
      **---------------------------------------------------------------**
       77 a-fl-ia                       pic x(01).
      *******************************************************************
      * Identifica la modalita` attuale: (S)elezione,(A)ggiornamento    *
      *******************************************************************
       77 a-modalita                    pic x(01).

       77 a-i                           pic 9(04).

       77  a1-prog                      pic 9(04).
       77  a1-n-val                     pic 9(04).

       77  a2-tip                       pic x(01).
       77  a2-id                        pic 9(04).

       77 a3-v-pos-sav                  pic 9(04)v99.
       77 a3-h-pos-sav                  pic 9(04)v99.
       77 a3-move-v                     pic s9(04)v99.
       77 a3-move-h                     pic s9(04)v99.
       77 a3-i                          pic 9(04).

       77 a3-padre-riga-prec            pic 9(04).
       77 a3-padre-riga-dopo            pic 9(04).
       77 a3-padre-riga-att             pic 9(04).

       77  a4-prog-da                   pic 9(04).
       77  a4-prog-a                    pic 9(04).
       copy "scrfldvf.cpy" replacing leading "flv-" by "a4-flv-".

       01 c-padre                       pic 9(04).

       01 d-liv-prec                    pic x(02).
       01 d-fld-tip-prec                pic x(01).

       77 e-pagine                      pic 9(08).
       77 e-errori                      pic 9(08).

       01 e1-form.
         02 e1-pac                      pic x(08).
         02 e1-prg                      pic x(20).
         02 e1-frm                      pic 9(02).
       01 e1-pag                        pic 9(02).

       01 e1-padre                      pic 9(04).
       01 e1-liv-prec                   pic x(02).
       01 e1-fld-tip-prec               pic x(01).
       01 w-clipboard                   pic x(64000).
       01 buf-size                      signed-int.
       77 handle-ef                     handle of entry-field.

       77 w-colore-salvato              pic 9(08).
      *77 ext-scr-dir                   pic x(70) is external.
       77 ext-out-dir                   pic x(70) is external.
       77 ext-lav-cbl-dir-client        pic x(70) is external.
       77 ext-lav-cbl-dir               pic x(70) is external.
       77 ext-lav-cbl-dir-server        pic x(70) is external.
       77 i                             pic 9(04).
       77 i-rif                         pic 9(04).
       77 i-prec                        pic 9(04).
       77 k                             pic 9(04).
       77 w-9-1                         pic 9(01).
       77 w-9-2                         pic 9(02).
       77 x-prg                         pic x(20).
       77 x-silent-mode                 pic x(01).
       77 x-prg-rigenerati              pic 9(08).
       77 x-rigenera-risorse            pic x(01).
       77 c-ok                          pic x(01).
       77 nf-name-tab                   pic x(70).
       77 zeta3                         pic zzz.
       77 zeta8a                        pic zzzzzzzz.
       77 zeta8b                        pic zzzzzzzz.
       77 tm-enabled-salva              pic x(01).
       01 w-dati-called.
         02 w-dc-called                 pic x(01).
      *
       77 fl-aggiungi-files             pic x(01).
       77 fl-ins-var                    pic x(01).
       77 fl-ok                         pic x(01).

       01  nf-name-1                    pic x(70).
       01  nf-name-2                    pic x(70).
       01  nf-name-11                   pic x(70).
      *
       78 k-crm-files                   value 7.
       01  name-files-crm.
         03 f-crm-num-files        pic 9(03) value k-crm-files.
         03 f-crm-tab-files.
           05 f-crm-name-1         pic x(70).
           05 filler               pic x(03) value '001'.
           05 f-crm-name-2         pic x(70).
           05 filler               pic x(03) value '002'.
           05 f-crm-name-3         pic x(70).
           05 filler               pic x(03) value '003'.
           05 f-crm-name-7         pic x(70).
           05 filler               pic x(03) value '007'.
           05 f-crm-name-11        pic x(70).
           05 filler               pic x(03) value '011'.
           05 f-crm-name-12        pic x(70).
           05 filler               pic x(03) value '012'.
           05 f-crm-name-13        pic x(70).
           05 filler               pic x(03) value '013'.
         03 f-crm-files redefines f-crm-tab-files.
           04 f-crm-ele occurs k-crm-files indexed by ind-f-crm.
             05 f-crm-name         pic x(70).
             05 f-crm-num          pic 9(03).
      *
       01 datasis.
         02 sis-anno                    pic 9(04).
         02 sis-mm                      pic 9(02).
         02 sis-gg                      pic 9(02).
      *******************************************************************
      * Record di interscambio con griglia                              *
      *******************************************************************
       01 grid-record.
         02 grd-prog                    pic zzzz.  
         02 grd-tml                     pic x(08).     
         02 grd-tml-des                 pic x(60).     
         02 grd-des                     pic x(60).
         02 grd-tip                     pic x(20).
         02 grd-liv                     pic x(20).
         02 grd-handle                  pic x(40).
         02 grd-padre                   pic zzzz.  
         02 grd-size                    pic zzzz.  
         02 grd-size-dec                pic zz.  
         02 grd-v-size                  pic zzz9,99 blank when zero.
         02 grd-h-size                  pic zzz9,99 blank when zero.
         02 grd-v-pos                   pic zzz9,99 blank when zero.
         02 grd-h-pos                   pic zzz9,99 blank when zero.
         02 grd-id                      pic zzzz.
         02 grd-fl-value                pic x(06).
         02 grd-enabled                 pic x(10).
         02 grd-visible                 pic x(10).
         02 grd-align                   pic x(06).
         02 grd-label                   pic x(30).
         02 grd-fl-grid-frame           pic x(10).
         02 grd-fl-hnd-label            pic x(10).
         02 grd-case                    pic x(15).
         02 grd-layout                  pic x(40).
         02 grd-frame-style             pic x(10).
         02 grd-fl-full-height          pic x(12).
         02 grd-fl-notify               pic x(06).
         02 grd-fl-cent-head            pic x(10).
         02 grd-fl-check-true           pic x(02).
         02 grd-fl-check-false          pic x(02).
         02 grd-fl-val-label            pic x(10).
         02 grd-s67-liv-ric             pic x(01).
         02 grd-u10-divisa              pic x(20).
         02 grd-u10-data                pic x(20).
         02 grd-u10-tipo-dato           pic x(10).
         02 grd-s52-verifica            pic x(01).
         02 grd-fl-entry-point          pic x(02).
         02 grd-s93-verifica            pic x(01).
         02 grd-su                      pic x(02).
         02 grd-giu                     pic x(03).
         02 grd-id-assoluto             pic z(08).
         02 grd-hc-id                   pic zzz.zzz.zzz.zzz.
         02 grd-hc-attivo               pic x(02).
         02 grd-fldx-id-controllo       pic z(04).
      *
       01 tab-maschera.
         02 tm-pagina.
           03 tm-pac                    pic x(08).
           03 tm-prg                    pic x(20).
           03 tm-frm                    pic 9(02).
           03 tm-pag                    pic 9(02).
         02 tm-pac-des                  pic x(40).
         02 tm-pac-cbl-dir              pic x(70).
         02 tm-pac-cbl-dir-client       pic x(70).
         02 tm-pac-cob-dir              pic x(70).
         02 tm-prg-des                  pic x(40).
         02 tm-pag-des                  pic x(20).
         02 tm-cbl-esiste               pic x(01).

         02 tm-ges-tabella              pic x(01).
         02 tm-nome-tabella             pic x(20).
         02 tm-versione                 pic 9(04).

         02 tm-ges-lingua               pic x(01).
         02 tm-ges-custom               pic x(01).
         02 tm-ges-wd2                  pic x(01).
         02 tm-dati.
           03 tm-frm-dati.
             04 tm-frm-des              pic x(40).
             04 tm-frm-v-size           pic 9(04)v99.
             04 tm-frm-h-size           pic 9(04)v99.
           03 tm-ele-sav                pic x(1000).
           03 tm-pnt                    pic 9(04).
           03 tm-massimo                pic 9(04).
           03 tm-id-max                 pic 9(04).
           03 tm-v-pos-max              pic 9(04)v99.
           03 tm-h-pos-max              pic 9(04)v99.
           03 tm-h-pos-fine-max         pic 9(04)v99.
           03 tm-sel-idx                pic 9(04).
           03 tm-sel-prog               pic 9(04).
           03 tm-v-pos-sav              pic 9(04)v99.
           03 tm-h-pos-sav              pic 9(04)v99.

           03 tm-column-id              pic 9(04).
           03 tm-column-su              pic 9(04).
           03 tm-column-giu             pic 9(04).
           03 tm-column-liv             pic 9(04).
         02 tm-tab.
           03 tm-righe occurs k-max-ele-tab times.
             04 tm-ele.
               05 tm-prog               pic 9(04).
               05 tm-n-val              pic 9(02).
               05 tm-tml                pic x(08).
               05 tm-tml-des            pic x(40).
               05 tm-des                pic x(60).
               05 tm-tip                pic x(01).
               05 tm-pb-bitmap          pic x(01).
               05 tm-size               pic 9(08).
               05 tm-size-dec           pic 9(02).
               05 tm-v-size             pic 9(04)v99.
               05 tm-h-size             pic 9(04)v99.
               05 tm-v-pos              pic 9(04)v99.
               05 tm-h-pos              pic 9(04)v99.
               05 tm-id                 pic 9(04).
               05 tm-id-assoluto        pic 9(08).
               05 tm-fldx-id-controllo  pic 9(04).
               05 tm-nome               pic x(30).
               05 tm-fl-value           pic x(01).
               05 tm-indice             pic x(08).
               05 tm-enabled            pic x(01).
               05 tm-visible            pic x(01).
               05 tm-align              pic x(01).
               05 tm-label              pic x(60).
               05 tm-label-h-pos-rel    pic s9(03)v99.
               05 tm-label-v-pos-rel    pic s9(03)v99.
               05 tm-label-h-size       pic 9(03)v99.
               05 tm-label-v-size       pic 9(03)v99.
               05 tm-lab-def            pic x(15).
               05 tm-liv                pic x(02).
               05 tm-color-control      pic 9(08).
               05 tm-color-label        pic 9(08).
               05 tm-padre              pic 9(04).
               05 tm-fl-grid-frame      pic x(01).
               05 tm-fl-hnd-label       pic x(01).
               05 tm-case               pic x(01).
               05 tm-layout             pic x(40).
               05 tm-css-classe         pic x(30).
               05 tm-frame-style        pic x(01).
               05 tm-fl-full-height     pic x(01).
               05 tm-fl-notify          pic x(01).
               05 tm-fl-self-act        pic x(01).
               05 tm-fl-headings        pic x(01).
               05 tm-fl-cent-head       pic x(01).
               05 tm-check-true         pic x(02).
               05 tm-check-false        pic x(02).
               05 tm-fl-val-label       pic x(01).
               05 tm-fl-lab-def         pic x(01).
               05 tm-s67-liv-ric        pic 9(01).
               05 tm-u10-divisa         pic x(20).
               05 tm-u10-data           pic x(20).
               05 tm-u10-tipo-dato      pic x(01).
               05 tm-u10-edit-punti     pic x(01).
               05 tm-s52-verifica       pic x(01).
               05 tm-fl-entry-point     pic x(01).
               05 tm-fl-grid-dinamica   pic x(01).
               05 tm-fl-edit-grid       pic x(01).
               05 tm-fl-color-form      pic x(01).
               05 tm-grid-prf-col       pic x(05).
               05 tm-grid-max-row       pic 9(06).
               05 tm-s93-verifica       pic x(01).
               05 tm-status-bar         pic x(80).
               05 tm-exception          pic x(10).
               05 tm-fl-sezione         pic x(01).
               05 tm-cond-enabled       pic x(500).
               05 tm-cond-visible       pic x(500).
               05 tm-enabled-auto       pic x(01).
               05 tm-visible-auto       pic x(01).
               05 tm-cond-row-color     pic x(300).
               05 tm-gor-alt-key        pic x(300).
               05 tm-fl-evidenza        pic x(01).
               05 tm-fl-secure          pic x(01).
               05 tm-fl-pos-man         pic x(01).
               05 tm-grid-ctrl-a-ep     pic x(01).
               05 tm-grid-be-ep         pic x(01).
               05 tm-hc-id              pic 9(12).
               05 tm-hc-attivo          pic x(01).
               05 tm-disattiva-tf-ep    pic x(01).
      *
       copy "wgrave.cpy".
       copy "wgrid.cpy".
       copy "wnscr.cpy".
       copy "wopenf.cpy".
       copy "wstato.cpy".
       copy "cogazien.cpy".
       copy "utilscrdup.cpy".
       copy "utilscrexp.cpy".
       copy "utilscrtml.cpy".
       copy "utilscrflv.cpy".
       copy "utilscrimp.cpy".
       copy "utilscrmge.cpy".
       copy "utilscrpgf.cpy".
       copy "utilscrpgm.cpy".
       copy "utilscrsim.cpy".
       copy "utilscrsrc.cpy".
       copy "utilscrtxt.cpy".
       copy "utilscrlng.cpy".
       copy "utilscrrel.cpy".
       copy "utilgcol.cpy".
       copy "utils68.cpy".
       copy "utilu20.cpy".
       copy "utilcoge0a.cpy".
       copy "utilscrpgnav.cpy".
       copy "utilscrers.cpy".
       copy "utilscrgtmpl.cpy".
       copy "utilscrgfdip.cpy".
       copy "utilscrgfil.cpy".
       copy "l-copiaf.cpy".
      *
       copy "utilgdad.cpy".
      *
       linkage section.
       copy "wcont.cpy".
       copy "utilgsis.cpy".
       copy "u-copiaf.cpy".
       copy "wcont1.cpy".
      *******************************************************************
      * Screen gestita dal programma                                    *
      *******************************************************************
       screen section.
       copy "scrfld-01.scr".
      *
       procedure division using stringhe.
       declaratives.
       io-error section.
           use after standard error procedure on 
            scrforms scrfmpag scrtempl scrfield scrfldvf scrpgmsg
            scrfldex scrfiles scrpgfil
            relpacch relprog  reldirpa
            .
       io-error-rout. exit.
       end declaratives.
      *
       main section.
       apri.
           move k-program-id             to prog-err w-nome-hlp
           perform ctr-abil
           perform z-99-init-program
           perform rd-azi

      **---------------------------------------------------------------**
      ** Apertura SCRFORMS
      **---------------------------------------------------------------**
           initialize w-nome-scrforms
           string ext-scr-dir         delimited " " 
              ext-os-slash "scrforms.arc" delimited size 
                                 into w-nome-scrforms
           open i-o scrforms
           if stato = "35"
              open output scrforms
              close scrforms
              open i-o scrforms
            else
              if stato not = "00"
                 string "Archivio non aperto: " w-nome-scrforms "; "
                    "stato : " stato  delimited size into wb-msg
                 perform vbx-msg-error
                 go to fine
              end-if
           end-if
      **---------------------------------------------------------------**
      ** Apertura SCRFMPAG
      **---------------------------------------------------------------**
           initialize w-nome-scrfmpag
           string ext-scr-dir         delimited " " 
              ext-os-slash "scrfmpag.arc" delimited size 
                                 into w-nome-scrfmpag
           open i-o scrfmpag
           if stato = "35"
              open output scrfmpag
              close scrfmpag
              open i-o scrfmpag
            else
              if stato not = "00"
                 string "Archivio non aperto: " w-nome-scrfmpag "; "
                    "stato : " stato  delimited size into wb-msg
                 perform vbx-msg-error
                 go to fine
              end-if
           end-if
      **---------------------------------------------------------------**
      ** Apertura SCRTEMPL
      **---------------------------------------------------------------**
           initialize w-nome-scrtempl
           string ext-scr-dir         delimited " " 
              ext-os-slash "scrtempl.arc" delimited size 
                                 into w-nome-scrtempl
           open i-o scrtempl
           if stato = "35"
              open output scrtempl
              close scrtempl
              open i-o scrtempl
            else
              if stato not = "00"
                 string "Archivio non aperto: " w-nome-scrtempl "; "
                    "stato : " stato  delimited size into wb-msg
                 perform vbx-msg-error
                 go to fine
              end-if
           end-if
      **---------------------------------------------------------------**
      ** Apertura SCRFIELD
      **---------------------------------------------------------------**
           initialize w-nome-scrfield
           string ext-scr-dir         delimited " " 
              ext-os-slash "scrfield.arc" delimited size 
                                 into w-nome-scrfield
           open i-o scrfield
           if stato = "35"
              open output scrfield
              close scrfield
              open i-o scrfield
            else
              if stato not = "00"
                 string "Archivio non aperto: " w-nome-scrfield "; "
                    "stato : " stato  delimited size into wb-msg
                 perform vbx-msg-error
                 go to fine
              end-if
           end-if
      **---------------------------------------------------------------**
      ** Apertura SCRFLDVF
      **---------------------------------------------------------------**
           initialize w-nome-scrfldvf
           string ext-scr-dir         delimited " " 
              ext-os-slash "scrfldvf.arc" delimited size 
                                 into w-nome-scrfldvf
           open i-o scrfldvf
           if stato = "35"
              open output scrfldvf
              close scrfldvf
              open i-o scrfldvf
            else
              if stato not = "00"
                 string "Archivio non aperto: " w-nome-scrfldvf "; "
                    "stato : " stato  delimited size into wb-msg
                 perform vbx-msg-error
                 go to fine
              end-if
           end-if
      **---------------------------------------------------------------**
      ** Apertura SCRPGMSG
      **---------------------------------------------------------------**
           initialize w-nome-scrpgmsg
           string ext-scr-dir         delimited " " 
              ext-os-slash "scrpgmsg.arc" delimited size 
                                 into w-nome-scrpgmsg
           open i-o scrpgmsg
           if stato = "35"
              open output scrpgmsg
              close scrpgmsg
              open i-o scrpgmsg
            else
              if stato not = "00"
                 string "Archivio non aperto: " w-nome-scrpgmsg "; "
                    "stato : " stato  delimited size into wb-msg
                 perform vbx-msg-error
                 go to fine
              end-if
           end-if
      **---------------------------------------------------------------**
      ** Apertura SCRFLDEX
      **---------------------------------------------------------------**
           initialize w-nome-scrfldex
           string ext-scr-dir         delimited " " 
              ext-os-slash "scrfldex.arc" delimited size 
                                 into w-nome-scrfldex
           open i-o scrfldex
           if stato = "35"
              open output scrfldex
              close scrfldex
              open i-o scrfldex
            else
              if stato not = "00"
                 string "Archivio non aperto: " w-nome-scrfldex "; "
                    "stato : " stato  delimited size into wb-msg
                 perform vbx-msg-error
                 go to fine
              end-if
           end-if
      **---------------------------------------------------------------**
      ** Apertura SCRFILES
      **---------------------------------------------------------------**
           initialize w-nome-scrfiles
           string ext-scr-dir         delimited " " 
              ext-os-slash "scrfiles.arc" delimited size 
                                 into w-nome-scrfiles
           open input scrfiles
           if stato = "35"
              open output scrfiles
              close scrfiles
              open input scrfiles
            else
              if stato not = "00"
                 string "Archivio non aperto: " w-nome-scrfiles "; "
                    "stato : " stato  delimited size into wb-msg
                 perform vbx-msg-error
                 go to fine
              end-if
           end-if
      **---------------------------------------------------------------**
      ** Apertura SCRPGFIL
      **---------------------------------------------------------------**
           initialize w-nome-scrpgfil
           string ext-scr-dir         delimited " " 
              ext-os-slash "scrpgfil.arc" delimited size 
                                 into w-nome-scrpgfil
           open i-o scrpgfil
           if stato = "35"
              open output scrpgfil
              close scrpgfil
              open i-o scrpgfil
            else
              if stato not = "00"
                 string "Archivio non aperto: " w-nome-scrpgfil "; "
                    "stato : " stato  delimited size into wb-msg
                 perform vbx-msg-error
                 go to fine
              end-if
           end-if

      **---------------------------------------------------------------**
      ** Apertura RELPACCH
      **---------------------------------------------------------------**
           initialize nf-name-1
           string ext-rel-dir         delimited " " 
              ext-os-slash "relpacch.arc" delimited size 
                                 into nf-name-1
           open i-o relpacch
           if stato not = "00"
              string "Archivio non aperto: " nf-name-1 "; "
                 "stato : " stato  delimited size into wb-msg
              perform vbx-msg-error
              go to fine
           end-if

      **---------------------------------------------------------------**
      ** Apertura RELPROG
      **---------------------------------------------------------------**
           initialize nf-name-2
           string ext-rel-dir         delimited " " 
              ext-os-slash "relprog.arc" delimited size 
                                 into nf-name-2
           open i-o relprog
           if stato not = "00"
              string "Archivio non aperto: " nf-name-2 "; "
                 "stato : " stato  delimited size into wb-msg
              perform vbx-msg-error
              go to fine
           end-if

      **---------------------------------------------------------------**
      ** Apertura RELDIRPA
      **---------------------------------------------------------------**
           initialize nf-name-11
           string ext-rel-dir         delimited " " 
              ext-os-slash "reldirpa.arc" delimited size 
                                 into nf-name-11
           open i-o reldirpa
           if stato not = "00"
              string "Archivio non aperto: " nf-name-11 "; "
                 "stato : " stato  delimited size into wb-msg
              perform vbx-msg-error
              go to fine
           end-if

           initialize scrsimfl-handle
           initialize scrsimfp-handle

      *******************************************************************
      * Definizione t.f. utilizzabili                                   *
      *******************************************************************
      *    move 1                        to w-presenza-invio
      *    move 1                        to w-presenza-invio-modifica
           if w-dc-called = "S"
              move 1                     to w-presenza-f3-selezione
              move 1                     to w-presenza-f3-modifica
           end-if
           move 1                        to w-presenza-f3-conferma
           move 1                        to w-presenza-f4-cancella
           move 1                        to w-presenza-f5-salva-come
           move 1                        to w-presenza-f7-verifica
           move 1                        to w-presenza-f8-aiuto
           move 1                        to w-presenza-sf4-griglia
           move 1                        to w-presenza-cta-aggiungi
           move 1                        to w-presenza-cte-elimina
           move 1                        to w-presenza-cti-inserisci

           accept datasis              from century-date

      **---------------------------------------------------------------**
      * Caricamento bitmap custom
      **---------------------------------------------------------------**
           initialize w-push-button-custom
           move 1                        to w-pbc-idx-handle
           move "file"                   to w-pbc-bitmap-file 
                                           (w-pbc-idx-handle)
           move 2                        to w-pbc-idx-handle
           move "screens"                to w-pbc-bitmap-file
                                           (w-pbc-idx-handle)
           perform load-bitmap-custom
      **---------------------------------------------------------------**
      * Crazione pulsanti custom
      **---------------------------------------------------------------**
           move 2                        to w-pbc-idx-handle
           move 5                        to w-pbc-bitmap-number
           move "Note su campo (F2)"     to w-pbc-title
           move k-f2                     to w-pbc-exception
           perform x-display-push-button-custom
           move 2                        to w-pbc-idx-handle
           move 1                        to w-pbc-bitmap-number
           move "Gestione files (F6)"    to w-pbc-title
           move k-f6                     to w-pbc-exception
           perform x-display-push-button-custom
           move 1                        to w-pbc-idx-handle
           move 2                        to w-pbc-bitmap-number
           move "Trasporta (F7)"         to w-pbc-title
           move k-f7                     to w-pbc-exception
           perform x-display-push-button-custom
           move "N"                      to w-pbc-fl-add
           move 2                        to w-pbc-idx-handle
           move 2                        to w-pbc-bitmap-number
           move "Gestione messaggi (F9)" to w-pbc-title
           move k-f9                     to w-pbc-exception
           perform x-display-push-button-custom
           move 2                        to w-pbc-idx-handle
           move 3                        to w-pbc-bitmap-number
           move "Chiamata a Gridsist (F10)" to w-pbc-title
           move k-f10                    to w-pbc-exception
           perform x-display-push-button-custom
           move 2                        to w-pbc-idx-handle
           move 4                        to w-pbc-bitmap-number
           move "Etichette di stampa (F11)" to w-pbc-title
           move k-f11                    to w-pbc-exception
           perform x-display-push-button-custom
           move 2                        to w-pbc-idx-handle
           move 6                        to w-pbc-bitmap-number
           move "Importa programma (F4)" to w-pbc-title
           move k-f4                     to w-pbc-exception
           perform x-display-push-button-custom
           .

           move "Configurazione forms"   to titolo-maschera
           move k-m-v-size               to m-v-size
           move k-m-h-size               to m-h-size
           perform rd-msk-standard
           move 1                        to i
           .
      *******************************************************************
      * Se il programma e` richiamato, lo pongo in modalita` "selezione"*
      *******************************************************************
           if w-dc-called = "S"
              move "S"                   to a-modalita
           else
              move "A"                   to a-modalita
           end-if
           move "S"                      to fl-esc-agg

           display s-mm-1

           perform z-prepara-tm-grid
           perform z-costruisci-controlli
           initialize tab-maschera
           move "N"                      to tm-ges-tabella
           move "N"                      to tm-ges-lingua
           move "N"                      to tm-ges-custom
           move "N"                      to tm-ges-wd2
           .
       a4.
           initialize tm-dati tm-tab

           modify e-tab-control, value = 1
           set event-type                to cmd-tabchanged
           set event-data-1              to 1
      *    perform z-99-event-form
           perform x-cambia-pagina

           initialize w-tab-pagine
           move 1                        to w-tab-current-page
           move zero                     to w-tab-current-page-test
           move 1                        to w-tab-pag-visible (1)
           perform x-display-pagina

           perform x-destroy-menu
      *----------------------------------------------------------------
      * Preparazione MAIN MENU
      *----------------------------------------------------------------
           perform x-crea-menu
           call "W$MENU"           using wmenu-show, thm-main
                                  giving myResult

      **---------------------------------------------------------------**
      ** Cerco le colonne "su" e "giu"
      **---------------------------------------------------------------**
           move 0                        to k
           perform with test after until w-hid-data-col-id = spaces
              add 1                      to k
              inquire e-tm-tab (1, k), hidden-data in w-hidden-data
              evaluate w-hid-data-col-id 
               when "id"
                 move k                  to tm-column-id
               when "su"
                 move k                  to tm-column-su
               when "giu"
                 move k                  to tm-column-giu
               when "liv"
                 move k                  to tm-column-liv
              end-evaluate
              inquire e-tm-tab (1, k), cell-data in w-hidden-data
           end-perform

           initialize x-enabled
           modify e-frame-dettaglio, title = "Dettaglio"
           perform x-status-bar-init
      *    move "EUROCOGE"         to tm-pac
      *    move "COGE08C"          to tm-prg
      *    move "CRM016"           to tm-prg
           perform z-carica-controlli
           move 1                  to tm-frm
           modify e-tm-frm, value = tm-frm
           move 0                        to tm-pnt
           perform z-costruisci-grid


      *    move "Eseguo scansione?" to wb-msg
      *    perform acc-conf-custom
      *    if f3
      *       perform e-scan              
      *    end-if
           .
      **---------------------------------------------------------------**
      ** Pacchetto
      **---------------------------------------------------------------**
       a-tm-pac.
           move "Inserire il pacchetto o premere F8 per aiuto/gestione"
                                         to sb-msg
           perform x-status-bar-msg
           perform x-attiva-f8-aiuto
           move k-f7-trasporta     to w-pbc-idx
           perform x-attiva-pbc
           accept e-tm-pac               on exception continue.
           perform z-99-exception-form
           move k-f7-trasporta     to w-pbc-idx
           perform x-disattiva-pbc
           if z-exception-prosegui = "N"
              go to a-tm-pac
           end-if
           perform x-disattiva-f8-aiuto
           if f-event
              go to x-test-mouse
           end-if
           if wesc
              perform acc-conf
              if f3
                 go to fine
              end-if
           end-if

           if f7
              initialize util-screxp
              move "Importa"       to screxp-ope
              move "SCREXP-CALLED" to screxp-called
              move prog-err        to screxp-caller
              move tm-pac          to screxp-pac
              move tm-prg          to screxp-prg
              call "SCREXP"     using stringhe util-screxp
              cancel "SCREXP"
              go to a-tm-pac
           end-if

           perform x-controlla-tm-pac
           if x-f3-ok-parziale = "N"
              move x-f3-msg              to wb-msg
              perform box-msg
              go to a-tm-pac
           end-if

           if tm-pac = "EUROASSI"
              string "Attenzione: il pacchetto EUROASSI non e' piu' in"
                " uso; " k-newline "quindi: " k-newline
                "1) salvare la maschera in EUROCOGE" k-newline
                "2) rimuovere la maschera da EUROASSI"
                 delimited size       into wb-msg
              perform vbx-msg-info
           end-if

           .
      **---------------------------------------------------------------**
      ** Programma
      **---------------------------------------------------------------**
       a-tm-prg.
           move "Inserire il programma o premere F8 per aiuto/gestione"
                                         to sb-msg
           perform x-status-bar-msg
           perform x-attiva-f8-aiuto
           accept e-tm-prg               on exception continue.
           perform z-99-exception-form
           if z-exception-prosegui = "N"
              go to a-tm-prg
           end-if
           perform x-disattiva-f8-aiuto
           if f-event
              go to x-test-mouse
           end-if
           if wesc
              go to a4
           end-if
           if f2                go to a-tm-pac.

           perform x-controlla-tm-prg
           if x-f3-ok-parziale = "N"
              move x-f3-msg              to wb-msg
              perform box-msg
              go to a-tm-prg
           end-if


           initialize scr-rec
           move tm-pac                   to scr-pac
           move tm-prg                   to scr-prg
           move 1                        to scr-frm
           perform rd-scrforms
           move scr-ges-lingua           to tm-ges-lingua
           if tm-ges-lingua not = "S"
              move "N"                   to tm-ges-lingua
           end-if
           move scr-ges-tabella          to tm-ges-tabella
           if tm-ges-tabella not = "S"
              move "N"                   to tm-ges-tabella
           end-if
           if scr-versione not numeric
              move 0                    to scr-versione
           end-if
           move scr-versione             to tm-versione
           move scr-ges-custom           to tm-ges-custom
           if tm-ges-custom not = "S"
              move "N"                   to tm-ges-custom
           end-if
           move scr-ges-wd2              to tm-ges-wd2
           if tm-ges-wd2 not = "S"
              move "N"                   to tm-ges-wd2
           end-if

      * Stabilisco se il nome del programma corrisponde ad una tabella;
      * se si, il programma sara' di gestione tabella
           if tm-prg (1:1) = "Y"
              move "N"                     to tm-ges-tabella

              move 0                    to k
              perform 26 times
                 add 1                  to k
                 initialize gfil-rec
                 move function lower-case(tm-prg) to tm-nome-tabella
                 move talf-lettera (k)  to tm-nome-tabella (1:1) 
                 move tm-nome-tabella         to gfil-nome
                 perform rd-scrfiles
                 if w-verbo-ok
                    move "Gestione tabella"   to wb-msg
                    perform vbx-msg-info
                    move "S"                  to tm-ges-tabella
                    move "S"                  to tm-ges-lingua
                    move "S"                  to tm-ges-custom
                    move 25                   to tm-frm-v-size
                    move 80                   to tm-frm-h-size
                    modify e-tm-frm-v-size, value = tm-frm-v-size
                    modify e-tm-frm-h-size, value = tm-frm-h-size
                    exit perform
                 end-if
              end-perform
           end-if

           move "N"                      to tm-cbl-esiste
           initialize prg-rec
           move tm-pac                   to prg-pack
           move tm-prg                   to prg-prg
           move "CBL"                    to prg-ext
           perform rd-relprog
           if w-verbo-ok
              move "S"                   to tm-cbl-esiste
            else
              if tm-ges-tabella = "S"
                 move "Programma non esistente in RELEASE. Lo genero?"
                                        to wb-msg
                 perform acc-conf-custom
                 if f3
                     move prg-pack      to prg-pack3
                     move prg-ext       to prg-ext3
                     move prg-prg       to prg-prg3

                     move " "           to prg-des
                     string "Gestione tabella " tm-nome-tabella
                        delimited size    into prg-des
                     move "-Si W32"     to prg-opz-cmp
                    
                    perform wr-relprog
                    move "S"            to tm-cbl-esiste
                 end-if
               else
                 move "Programma non esistente!!" to wb-msg
                 perform vbx-msg-warning
              end-if
           end-if
      * Ripeto il controllo con il COB
           initialize prg-rec
           move tm-pac                   to prg-pack
           move tm-prg                   to prg-prg
           move "COB"                    to prg-ext
           perform rd-relprog
           if w-verbo-invalido
              if tm-ges-tabella = "S" and 
                 tm-cbl-esiste  = "S"

                 move prg-pack      to prg-pack3
                 move prg-ext       to prg-ext3
                 move prg-prg       to prg-prg3

                 move " "           to prg-des
                 string "Eseguibile del programma omonimo"
                    delimited size    into prg-des
                 perform wr-relprog
              end-if
           end-if

           move prg-des                  to tm-prg-des
           display e-tm-prg-des

           perform z-carica-s95
           .
      *
      *---------------------------------------------------------------*
      * Gestione lingua?
      *---------------------------------------------------------------*
       a-tm-ges-lingua.
           move "Gestione descrizioni in lingua?" to sb-msg
           perform x-status-bar-msg
           initialize util-s95
           perform z-s95-tm-ges-lingua
           if f-event
              go to x-test-mouse
           end-if
           if wesc
              go to a4
           end-if
           if f2                go to a-tm-prg.

           perform x-controlla-tm-ges-lingua
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform vbx-msg-error
              go to a-tm-ges-lingua
           end-if
           .
      *
      *---------------------------------------------------------------*
      * Gestione customizzazioni?
      *---------------------------------------------------------------*
       a-tm-ges-custom.
           move "Gestione customizzazioni maschera?" to sb-msg
           perform x-status-bar-msg
           initialize util-s95
           perform z-s95-tm-ges-custom
      *--------------------------------------------------------------
      * La gestione customizzazione richiede la gestione lingua (la
      * chiamata alla routine di ricerca parametri customizzati avviene
      * in x-carica-lingua, per ridurre le implementazioni necessarie al
      * programma da customizzare)
      *--------------------------------------------------------------
           if tm-ges-custom  = "S" and
              tm-ges-lingua <> "S"
              move "S"             to tm-ges-lingua
              perform z-carica-s95
           end-if
           if f-event
              go to x-test-mouse
           end-if
           if wesc
              go to a4
           end-if
           if f2                go to a-tm-ges-lingua.

           perform x-controlla-tm-ges-custom
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform vbx-msg-error
              go to a-tm-ges-custom
           end-if
           .
      *
      *---------------------------------------------------------------*
      * Gestione WD2?
      *---------------------------------------------------------------*
       a-tm-ges-wd2.
           move "Gestione customizzazioni maschera?" to sb-msg
           perform x-status-bar-msg
           initialize util-s95
           perform z-s95-tm-ges-wd2
      *--------------------------------------------------------------
      * La gestione customizzazione richiede la gestione lingua (la
      * chiamata alla routine di ricerca parametri customizzati avviene
      * in x-carica-lingua, per ridurre le implementazioni necessarie al
      * programma da customizzare)
      *--------------------------------------------------------------
           if tm-ges-wd2  = "S" and
              tm-ges-lingua <> "S"
              move "S"             to tm-ges-lingua
              perform z-carica-s95
           end-if
           if f-event
              go to x-test-mouse
           end-if
           if wesc
              go to a4
           end-if
           if f2                go to a-tm-ges-lingua.

           perform x-controlla-tm-ges-wd2
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform vbx-msg-error
              go to a-tm-ges-wd2
           end-if
           .

      **---------------------------------------------------------------**
      ** Form                                                     
      **---------------------------------------------------------------**
       a-tm-frm.
           move "Inserire la form o premere F8 per aiuto/gestione"
                                         to sb-msg
           perform x-status-bar-msg
           perform x-attiva-f8-aiuto
           move k-f4-importa       to w-pbc-idx
           perform x-attiva-pbc
           move k-f6-file          to w-pbc-idx
           perform x-attiva-pbc
           move k-f7-trasporta     to w-pbc-idx
           perform x-attiva-pbc
           accept e-tm-frm               on exception continue.
           move k-f4-importa       to w-pbc-idx
           perform x-disattiva-pbc
           move k-f6-file          to w-pbc-idx
           perform x-disattiva-pbc
           move k-f7-trasporta     to w-pbc-idx
           perform x-disattiva-pbc
           perform x-disattiva-f8-aiuto
           perform z-99-exception-form
           if z-exception-prosegui = "N"
              go to a-tm-frm
           end-if
           if f-event
              go to x-test-mouse
           end-if
           if wesc
              go to a4
           end-if
           if f2                go to a-tm-ges-wd2.

           if f4
              initialize util-scrimp
              move tm-pac          to scrimp-pac
              move tm-prg          to scrimp-prg
              call "SCRIMP"     using stringhe util-scrimp
              cancel "SCRIMP"
              go to a-tm-frm
           end-if

           if f6
              initialize util-scrpgf
              move tm-pac          to scrpgf-pac
              move tm-prg          to scrpgf-prg
              call "SCRPGF"     using stringhe util-scrpgf
              cancel "SCRPGF"
              go to a-tm-frm
           end-if
           if f7
              initialize util-screxp
              move "SCREXP-CALLED" to screxp-called
              move prog-err        to screxp-caller
              move tm-pac          to screxp-pac
              move tm-prg          to screxp-prg
              call "SCREXP"     using stringhe util-screxp
              cancel "SCREXP"
              go to a-tm-frm
           end-if

           perform x-controlla-tm-frm
           if x-f3-ok-parziale = "N"
              move x-f3-msg              to wb-msg
              perform box-msg
              go to a-tm-frm
           end-if

           move scr-des                  to tm-frm-des
           if tm-ges-tabella <> "S"
              move scr-v-size            to tm-frm-v-size
              move scr-h-size            to tm-frm-h-size
           end-if
           modify e-tm-frm-des, value = tm-frm-des
           modify e-tm-frm-v-size, value = tm-frm-v-size
           modify e-tm-frm-h-size, value = tm-frm-h-size
           .
      **---------------------------------------------------------------**
      ** Descrizione form
      **---------------------------------------------------------------**
       a-tm-frm-des.
           move "Descrizione form" to sb-msg
           perform x-status-bar-msg
           if tm-frm-des = spaces
              if tm-frm = 1
                 move "Form principale" to tm-frm-des
                 modify e-tm-frm-des, value = tm-frm-des
              end-if
           end-if
           perform x-attiva-f5-salva-come
           accept e-tm-frm-des     on exception continue.
           modify e-tm-frm-des, color = ext-color-controls
           perform x-disattiva-f5-salva-come
           perform z-99-exception-form
           if z-exception-prosegui = "N"
              go to a-tm-frm-des
           end-if
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a4.
           if f2                go to a-tm-frm.

           if f5
              initialize util-scrdup
              move "SCRDUP-CALLED" to scrdup-called
              move prog-err        to scrdup-caller
              move tm-pac          to scrdup-pac
              move tm-prg          to scrdup-prg
              move tm-frm          to scrdup-frm
              call "SCRDUP"     using stringhe util-scrdup
              cancel "SCRDUP"
              go to a4
           end-if

           perform x-controlla-tm-frm-des
           if x-f3-ok-parziale = "N"
              move x-f3-msg              to wb-msg
              perform box-msg
              go to a-tm-frm-des
           end-if
           .
      **---------------------------------------------------------------**
      ** Linee form
      **---------------------------------------------------------------**
       a-tm-frm-v-size.
           move "Altezza in linee della form" to sb-msg
           perform x-status-bar-msg
           initialize util-u20
           perform z-u20-tm-frm-v-size
           perform z-99-exception-form
           if z-exception-prosegui = "N"
              go to a-tm-frm-v-size
           end-if
           if f-event
              go to x-test-mouse
           end-if
           if wesc
              go to a4
           end-if
           if f2                go to a-tm-frm-des.

           perform x-controlla-tm-frm-v-size
           if x-f3-ok-parziale = "N"
              move x-f3-msg              to wb-msg
              perform box-msg
              go to a-tm-frm-v-size
           end-if
           .
      **---------------------------------------------------------------**
      ** Colonne form
      **---------------------------------------------------------------**
       a-tm-frm-h-size.
           move "Larghezza in colonne della form" to sb-msg
           perform x-status-bar-msg
           initialize util-u20
           perform z-u20-tm-frm-h-size
           perform z-99-exception-form
           if z-exception-prosegui = "N"
              go to a-tm-frm-h-size
           end-if
           if f-event
              go to x-test-mouse
           end-if
           if wesc
              go to a4
           end-if
           if f2                go to a-tm-frm-v-size.

           perform x-controlla-tm-frm-h-size
           if x-f3-ok-parziale = "N"
              move x-f3-msg              to wb-msg
              perform box-msg
              go to a-tm-frm-h-size
           end-if
           .
      **---------------------------------------------------------------**
      ** Pagina
      **---------------------------------------------------------------**
       a-tm-pag.
           move "Inserire la pagina o premere F8 per aiuto/gestione"
                                         to sb-msg
           perform x-status-bar-msg
           perform x-attiva-f8-aiuto
           accept e-tm-pag               on exception continue.
           perform z-cambia-controlli
           perform z-99-exception-form
           if z-exception-prosegui = "N"
              go to a-tm-pag
           end-if
           perform x-disattiva-f8-aiuto
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a4.
           if f2                go to a-tm-frm-h-size.

           perform x-controlla-tm-pag
           if x-f3-ok-parziale = "N"
              move x-f3-msg              to wb-msg
              perform box-msg
              go to a-tm-pag
           end-if
           move fpg-des            to tm-pag-des
           modify e-tm-pag-des, value = tm-pag-des
      *    perform z-carica-controlli
           .
      **---------------------------------------------------------------**
      ** Descrizione pagina
      **---------------------------------------------------------------**
       a-tm-pag-des.
           move "Descrizione pagina"     to sb-msg
           perform x-status-bar-msg
           perform x-attiva-f5-salva-come
           accept e-tm-pag-des               on exception continue.
           modify e-tm-pag-des, color = ext-color-controls
           perform x-disattiva-f5-salva-come
           perform z-99-exception-form
           if z-exception-prosegui = "N"
              go to a-tm-pag-des
           end-if
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a4.
           if f2                go to a-tm-pag.

           perform x-controlla-tm-pag-des
           if x-f3-ok-parziale = "N"
              move x-f3-msg              to wb-msg
              perform box-msg
              go to a-tm-pag-des
           end-if
           perform z-carica-controlli
           .

      **---------------------------------------------------------------**
      ** Conferma dei dati di testata
      ** Se confermato ed e` stata richiesta la modifica di una pagina, 
      **   modifico anche il record relativo
      **---------------------------------------------------------------**
       a-conferma-pagina.
           perform x-imposta-conferma-pagina.
           if x-f3-ok = "N"
              move x-f3-msg              to wb-msg
              perform vbx-msg
              go to a-tm-pac
           end-if

           if tm-frm not = spaces
              initialize scr-rec
              move tm-pac                to scr-pac
              move tm-prg                to scr-prg
              move tm-frm                to scr-frm
              perform rd-scrforms
              if w-verbo-invalido
                 perform wr-scrforms
              end-if
              move tm-frm-des            to scr-des
              move tm-frm-v-size         to scr-v-size
              move tm-frm-h-size         to scr-h-size
              if tm-frm = 1
                 move tm-ges-tabella     to scr-ges-tabella
                 if tm-ges-tabella = "S"
                    move k-versione-ges-tabella to scr-versione
                 end-if
                 move tm-nome-tabella    to scr-nome-tabella
                 move tm-ges-lingua      to scr-ges-lingua
                 move tm-ges-custom      to scr-ges-custom
                 move tm-ges-wd2         to scr-ges-wd2     
              end-if
              perform rwr-scrforms
           end-if

           if tm-pag not = zero
              initialize fpg-rec
              move tm-pac                to fpg-pac
              move tm-prg                to fpg-prg
              move tm-frm                to fpg-frm
              move tm-pag                to fpg-pag
              perform rd-scrfmpag
              if w-verbo-invalido
                 perform wr-scrfmpag
              end-if
              move tm-pag-des            to fpg-des
              perform rwr-scrfmpag
           end-if

      *---------------------------------------------------------------**
      * Imposto il percorso per il sorgente da generare
      *---------------------------------------------------------------**
      * 1/12/11: Il percorso dei files di lingua dipende sempre dal
      *          cblconfi utilizzato (che dovrebbe coincidere sempre con 
      *          lo stesso ambiente)
      *---------------------------------------------------------------**
      * 22/5/17: provo a commentare questo, a SCRSIM non serve, quindi a
      * cosa server??
      *---------------------------------------------------------------**
           move tm-pac-cbl-dir-client    to ext-lav-cbl-dir-client
           move tm-pac-cbl-dir-client    to ext-lav-cbl-dir
           if ext-thin-client = "S"
              initialize util-string
              move function lower-case(ext-lav-cbl-dir) 
                                         to us-pub-object
              move "p:\"                 to us-pub-target
              move "/programmi/"         to us-pub-source
              perform string-replace
              move us-pub-object         to ext-lav-cbl-dir
              inspect ext-lav-cbl-dir replacing all "\" by "/"

      *       initialize util-string
      *       move function lower-case(ext-lng-dir) 
      *                                  to us-pub-object
      *       move "p:\"                 to us-pub-target
      *       move "/programmi/"         to us-pub-source
      *       perform string-replace
      *       move us-pub-object         to ext-lav-cbl-dir
      *       inspect ext-lav-cbl-dir replacing all "\" by "/"
           end-if

           if tm-ges-tabella = "S" and
              tm-pag         = 0
              perform f-template-gestione-tabella
           end-if

           .

       a-inizio-dati.
           perform d-carica-dati       thru ex-d
           move i                        to tm-pnt
           perform z-costruisci-grid

           inquire e-tm-tab, 
              cursor-y in i
           subtract 1                  from i
           if i > tm-pnt 
              move tm-pnt                to i
              modify e-tm-tab, cursor-y = i + 1
           end-if
           if i = 0
              move 1                     to i
           end-if
           perform z-carica-controlli
           perform x-attiva-tf-grid
           .
       a-grid.
           modify e-frame-dettaglio, title = "Dettaglio"
           initialize x-enabled
           perform x-disattiva-f3-conferma
           perform x-attiva-tf-grid
           modify e-tm-tab, enabled = 1

           move "Seleziona la riga ed esegui una delle operazioni"
                to sb-msg
           perform x-status-bar-msg
           .
       a-accept-grid.
           perform z-99-accept-grid

           perform z-99-exception-form

           if wesc
              perform x-disattiva-tf-grid
              go to a4
           end-if
           if f-event
              evaluate event-type 
               when msg-begin-entry
                 inquire e-tm-tab,
                   entry-reason          in w-entry-reason
                 if w-entry-double-click or
                    w-entry-enter
                    evaluate w-cell-x
      **-------------------------------------------------------------**
      ** Modifica ID 
      **-------------------------------------------------------------**
                     when tm-column-id
                        inquire e-tm-tab, cell-data in w-cell-num
                     when tm-column-su
                       if i not = 1
                          perform a3-sposta-su
                          modify e-tm-tab, cursor-y = i
                          go to a-inizio-dati
                       end-if
                     when tm-column-giu
                       if i not = tm-pnt
                          perform a3-sposta-giu
                          modify e-tm-tab, cursor-y = i + 2
                          go to a-inizio-dati
                       end-if
                     when other
                       set funzio           to k-fun-invio
                    end-evaluate
                 end-if
               when other
                 go to x-test-mouse
              end-evaluate
           end-if
           if s-f4
              initialize util-gdad
              move prog-err              to gdad-prg
              move "Configurazione schede" to gdad-prg-des
              move 1                     to gdad-frm
              move k-id-grid             to gdad-ctrl-id
              call "GRIDADMN" using stringhe util-gdad
              go to a-accept-grid
           end-if
      *******************************************************************
      * Se premo F3; se sono in modalita`:                              *
      *  - "Aggiornamento": salvo e ritorno in selezione                *
      *  - "Selezione": vado in aggiornamento                           *
      *******************************************************************
           if f3
              if a-modalita = "A"
                 if w-dc-called = "S"
                    perform x-disattiva-tf-grid
                    move "S"             to a-modalita
                 end-if
              else
                 perform x-disattiva-tf-grid
                 move "A"                to a-modalita
              end-if
              go to a-grid
           end-if

      **---------------------------------------------------------------**
      ** Cancellazione pagina
      **---------------------------------------------------------------**
           if f4
              move "Vuoi cancellare la pagina corrente?" to wb-msg
              perform acc-conf-custom
              if f3
                 initialize util-screxp
                 move "Cancella"      to screxp-ope
                 move "SCREXP-CALLED" to screxp-called
                 move prog-err        to screxp-caller
                 move tm-pac          to screxp-pac
                 move tm-prg          to screxp-prg
                 move tm-frm          to screxp-frm
                 move tm-pag          to screxp-pag
                 call "SCREXP"     using stringhe util-screxp
                 cancel "SCREXP"
                 go to a4
               else
                 go to a-grid
              end-if
           end-if

      **---------------------------------------------------------------**
      ** Duplicazione pagina
      **---------------------------------------------------------------**
           if f5
              initialize util-scrdup
              move "SCRDUP-CALLED" to scrdup-called
              move prog-err        to scrdup-caller
              move tm-pac          to scrdup-pac
              move tm-prg          to scrdup-prg
              move tm-frm          to scrdup-frm
              if tm-pag = 0
                 move 99           to scrdup-pag
               else
                 move tm-pag       to scrdup-pag
              end-if
              call "SCRDUP"     using stringhe util-scrdup
              cancel "SCRDUP"
              perform x-disattiva-tf-grid
              go to a4
           end-if

      **---------------------------------------------------------------**
      ** Gestione note su campo
      **---------------------------------------------------------------**
           if f2-fun
              initialize util-scrtxt
              move "Gestione"      to scrtxt-ope
              move tm-pac          to scrtxt-pac
              move tm-prg          to scrtxt-prg
              move "ita"           to scrtxt-lng
              move "NoteCampo"     to scrtxt-tip
              move tm-nome (i)     to scrtxt-id
              call "SCRTXT"     using stringhe util-scrtxt
              cancel "SCRTXT"
              go to a-accept-grid
           end-if

      **---------------------------------------------------------------**
      ** Gestione messaggi
      **---------------------------------------------------------------**
           if f9
              initialize util-scrpgm
              move tm-pac          to scrpgm-pac
              move tm-prg          to scrpgm-prg
              call "SCRPGM"     using stringhe util-scrpgm
              cancel "SCRPGM"
              go to a-accept-grid
           end-if

      **---------------------------------------------------------------**
      ** Gestione etichette di stampa
      **---------------------------------------------------------------**
           if f11
              initialize util-scrpgm
              move tm-pac          to scrpgm-pac
              move tm-prg          to scrpgm-prg
              move "S"             to scrpgm-tip
              call "SCRPGM"     using stringhe util-scrpgm
              cancel "SCRPGM"
              go to a-accept-grid
           end-if

      **---------------------------------------------------------------**
      ** Chiamata a GRIDSIST
      **---------------------------------------------------------------**
           if f10                               and
             (tm-tip (i) = k-ctr-grid       or
              tm-tip (i) = k-ctr-grid-paged   )
              initialize util-gsis
              move "GRIDSIST-CALLED"  to gsis-called
              move tm-prg          to gsis-prg
              move tm-frm          to gsis-form
              move tm-id (i)       to gsis-id
              call "GRIDSIST"   using stringhe 
              cancel "GRIDSIST"
              go to a-accept-grid
           end-if

      **---------------------------------------------------------------**
      ** Gestione files
      **---------------------------------------------------------------**
           if f6
              initialize util-scrpgf
              move tm-pac          to scrpgf-pac
              move tm-prg          to scrpgf-prg
              call "SCRPGF"     using stringhe util-scrpgf
              cancel "SCRPGF"
              go to a-accept-grid
           end-if

      **---------------------------------------------------------------**
      ** Verifica: lancio il programma di simulazione scheda
      **---------------------------------------------------------------**
           if f7
              initialize util-scrsim
              move "SCRSIM-CALLED" to scrsim-called
              move tm-pac          to scrsim-pac
              move tm-prg          to scrsim-prg
              move tm-prg-des      to scrsim-prg-des
              move tm-frm          to scrsim-frm
              move tm-pag          to scrsim-pag
              call "SCRSIM"     using stringhe util-scrsim
              cancel "SCRSIM"
              go to a-accept-grid
           end-if
           
      *******************************************************************
      * in base alla modalita` corrente, modifica riga o selezione      *
      *******************************************************************
           if invio
              if i > tm-pnt
                 go to a-accept-grid
              end-if
              if a-modalita = "A"
                 if w-abi-agg = "N"
                    move "Non sei abilitato a modificare in questa funzi
      -               "one!"             to wb-msg
                    perform vbx-msg-error
                    go to a-accept-grid
                 end-if
                 move "M"                to a-operazione
                 go to a-riga-dettaglio
              end-if
           end-if
      **---------------------------------------------------------------**
      ** Seleziono una riga, per poterla copiare in una nuova
      **---------------------------------------------------------------**
           if ctrl-s and
              a-modalita = "A"
              move i                     to tm-sel-idx
              move tm-prog (i)           to tm-sel-prog
              string "Riga selezionata: '" tm-des (i) "'"
                 delimited size        into wb-msg
              perform vbx-msg-info
              go to a-accept-grid
           end-if

      *******************************************************************
      * Aggiungo una riga                                               *
      *******************************************************************
           if ctrl-a and
              a-modalita = "A"
              if w-abi-ins = "N"
                 move "Non sei abilitato ad inserire in questa funzione!
      -           "!"                    to wb-msg
                 perform vbx-msg-error
                 go to a-accept-grid
              end-if
              move i                     to i-prec 
              move tm-pnt                to i-rif
              add 1                      to tm-pnt
              compute i = tm-pnt
              move "I"                   to a-operazione
              move "A"                   to a-fl-ia
              go to a-riga-dettaglio
           end-if
      *******************************************************************
      * Inserisco una riga alla posizione corrente
      *******************************************************************
           if ctrl-j and
              a-modalita = "A"
              if w-abi-ins = "N"
                 move "Non sei abilitato ad inserire in questa funzione!
      -           "!"                    to wb-msg
                 perform vbx-msg-error
                 go to a-accept-grid
              end-if
              move i                     to i-prec
              compute i-rif = i - 1
              add 1                      to tm-pnt
              compute i = tm-pnt
              move "I"                   to a-operazione
              move "I"                   to a-fl-ia
              go to a-riga-dettaglio
           end-if
      *******************************************************************
      * Elimino una riga                                                *
      *******************************************************************
           if ctrl-e and
              a-modalita = "A"
              if w-abi-del = "N"
                 move "Non sei abilitato a cancellare in questa funzione
      -           "!!"                   to wb-msg
                 perform vbx-msg-error
                 go to a-accept-grid
              end-if
              if tm-pnt = zero
                 move "Non ho righe da eliminare!!" to wb-msg
                 perform vbx-msg-error
                 go to a-grid
              end-if
              perform c1-ctrl-canc     thru ex-c1
              if c-ok = "N"
                 go to a-grid
              end-if
              move "Attenzione, vuoi eliminare la riga?" to wb-msg
              perform acc-conf-custom
              if f3
                 perform c-cancella    thru ex-c
                 go to a-inizio-dati
              end-if
           end-if
      *******************************************************************
      * Altre funzioni su dettaglio                                     *
      *******************************************************************
           go to a-accept-grid
           .
      *******************************************************************
      * Richiesta informazioni riga                                     *
      *******************************************************************
       a-riga-dettaglio.
           move 1                        to x-enabled-dettaglio
           perform x-disattiva-tf-grid
           perform x-attiva-f3-conferma
           modify e-tm-tab, enabled = 0

           if a-operazione = "M"
              modify e-tab-control, value = w-tab-current-page
              set event-type                to cmd-tabchanged
              set event-data-1              to w-tab-current-page
            else
              modify e-tab-control, value = 1
              set event-type                to cmd-tabchanged
              set event-data-1              to 1
           end-if
      *    perform z-99-event-form
           perform x-cambia-pagina
           perform z-carica-controlli
      *    display s-mm-1-tab-01
           perform z-cambia-controlli

      *******************************************************************
      * Se sono in variazione locko il record                           *
      *******************************************************************
           if a-operazione = "M"
              inquire e-tm-tab, cursor-y in w-cell-y
              modify e-frame-dettaglio, title = "Modifica riga"
              initialize fld-rec
              move tm-pac            to fld-pac   
              move tm-prg            to fld-prg   
              move tm-frm            to fld-frm   
              move tm-pag            to fld-pag   
              move tm-prog (i)       to fld-prog
              perform rd-scrfield-lk
              if tm-tip (i) = k-ctr-combo
                 move tm-prog (i)    to a1-prog
                 perform a1-conta-valori thru ex-a1
                 move a1-n-val     to tm-n-val (i)
              end-if
              if tm-tml (i) = spaces
                 move 1            to x-enabled-tm-tml
              end-if
              move tm-ele (i)      to tm-ele-sav
              move tm-v-pos (i)    to tm-v-pos-sav
              move tm-h-pos (i)    to tm-h-pos-sav
      **------------------------------------------------------------**
      ** Se sono in inserimento, e ho una riga "copiata", la incollo 
      **------------------------------------------------------------**
           else
              move tm-ele (i)        to tm-ele-sav
              compute w-cell-y = tm-pnt
              if a-fl-ia = "I"
                 modify e-frame-dettaglio, title = "Inserisci riga"
               else
                 modify e-frame-dettaglio, title = "Appendi riga"
              end-if
              move 1                     to x-enabled-tm-tml
              move "S"                   to tm-fl-value (i)
              move "S"                   to tm-u10-edit-punti (i)
              move "1"                   to tm-enabled (i)
              move "1"                   to tm-visible (i)
              move k-ctr-alfanumerico    to tm-tip (i)
              if tm-sel-idx not = 0
                 move tm-ele (tm-sel-idx) to tm-ele (i)
                 initialize tm-v-pos (i) tm-h-pos (i) tm-id (i) 
                            tm-id-assoluto (i)
              end-if
              compute tm-prog (i) = tm-massimo + 1
           end-if
           if tm-fl-grid-frame (i) not = "S" and
              tm-fl-grid-frame (i) not = "N" 
              move "N"                   to tm-fl-grid-frame (i)
           end-if
           if tm-fl-full-height (i) not = "S" and
              tm-fl-full-height (i) not = "N" 
              move "N"                   to tm-fl-full-height (i)
           end-if
           if tm-fl-notify (i) not = "S" and
              tm-fl-notify (i) not = "N" 
              move "N"                   to tm-fl-notify (i)
           end-if
           if tm-fl-self-act (i) not = "S" and
              tm-fl-self-act (i) not = "N" 
              move "N"                   to tm-fl-self-act (i)
           end-if
           if tm-fl-cent-head (i) not = "S" and
              tm-fl-cent-head (i) not = "N" 
              if tm-tip (i) not = k-ctr-grid and
                 tm-tip (i) not = k-ctr-grid-paged
                 move "N"                to tm-fl-cent-head (i)
              end-if
           end-if
           if tm-fl-hnd-label (i) not = "S" and
              tm-fl-hnd-label (i) not = "N" 
              move "N"                   to tm-fl-hnd-label (i)
           end-if
           if tm-fl-val-label (i) not = "S" and
              tm-fl-val-label (i) not = "N" 
              move "N"                   to tm-fl-val-label (i)
           end-if
           if tm-fl-entry-point (i) not = "S" and
              tm-fl-entry-point (i) not = "N" 
              move "N"                   to tm-fl-entry-point (i)
           end-if
           if tm-fl-grid-dinamica (i) not = "S" and
              tm-fl-grid-dinamica (i) not = "N" 
              move "S"                   to tm-fl-grid-dinamica (i)
           end-if
           if tm-fl-edit-grid (i) not = "S" and
              tm-fl-edit-grid (i) not = "A" and
              tm-fl-edit-grid (i) not = "N" 
              move "N"                   to tm-fl-edit-grid (i)
           end-if
           if tm-fl-color-form (i) not = "S" and
              tm-fl-color-form (i) not = "N" 
              move "N"                   to tm-fl-color-form (i)
           end-if
           if tm-fl-sezione (i) not = "S" and
              tm-fl-sezione (i) not = "N" 
              move "N"                   to tm-fl-sezione (i)
           end-if
           if tm-pb-bitmap (i) not = "S" and
              tm-pb-bitmap (i) not = "N" 
              move "N"                   to tm-pb-bitmap (i)
           end-if
           if tm-fl-evidenza (i) not = "S" and
              tm-fl-evidenza (i) not = "N" 
              move "N"                   to tm-fl-evidenza (i)
           end-if
           if tm-fl-secure (i) not = "S" and
              tm-fl-secure (i) not = "N" 
              move "N"                   to tm-fl-secure (i)
           end-if
           if tm-fl-pos-man (i) not = "S" and
              tm-fl-pos-man (i) not = "N" 
              move "N"                   to tm-fl-pos-man (i)
           end-if
           if tm-grid-ctrl-a-ep (i) <> "S" and
              tm-grid-ctrl-a-ep (i) <> "N" 
              move "N"                   to tm-grid-ctrl-a-ep (i)
           end-if
           if tm-grid-be-ep (i) <> "S" and
              tm-grid-be-ep (i) <> "N" 
              move "N"                   to tm-grid-be-ep (i)
           end-if
           if tm-disattiva-tf-ep (i) <> "S" and
              tm-disattiva-tf-ep (i) <> "N" 
              move "N"                   to tm-disattiva-tf-ep (i)
           end-if

           perform z-carica-s95
           display s-mm-1-tab-01
           perform z-forza-cambia-controlli
           evaluate w-tab-current-page
            when 1
              go to a-pagina-01
            when 2
              go to a-pagina-02
            when 3
              go to a-pagina-03
            when 4
              go to a-pagina-04
            when other
           end-evaluate
      *    move tm-ele (i)               to tm-ele-sav
           .
       a-pagina-01.
           move 1                        to w-tab-current-page-test
           perform z-carica-vari
           display s-mm-1-tab-01
           initialize event-type
           .

      **---------------------------------------------------------------**
      ** Template di riferimento
      **---------------------------------------------------------------**
       a-tm-tml.
           move "Template a cui fa riferimento il controllo" to sb-msg
           perform x-status-bar-msg
           perform x-attiva-f8-aiuto
           accept e-tm-tml           on exception continue.
           perform x-disattiva-f8-aiuto
           modify e-tm-tml, color = ext-color-controls
           if f-event
              go to x-test-mouse
           end-if
           .
      **---------------------------------------------------------------**
      * Se premo ESC ora annullo le modifiche della riga                *
      **---------------------------------------------------------------**
       a-tm-tml-tf.
           if wesc
              if tm-tip (i)   = k-ctr-combo and
                 a-operazione = "M"
                 if tm-n-val (i) < 2
                    string "Attenzione: durante la modifica i valori "
                     "fissi di questa" k-newline "informazione sono "
                     "diventati meno di 2. Ripristinarli." 
                      delimited size  into wb-msg
                    perform vbx-msg-error
                    go to a-tm-tml
                 end-if
              end-if
              move "Attenzione, vuoi annullare le modifiche della riga?"
                                         to wb-msg
              perform acc-conf-custom
              if f3
                 move tm-ele-sav         to tm-ele (i)
                 if a-operazione = "I"
                    subtract 1         from tm-pnt
                    move i-prec          to i
                 end-if
                 display s-mm-1-tab-01
                 perform z-forza-cambia-controlli
                 modify e-tm-tab,
                    cursor-y = i + 1,
                    cursor-x = 1
                 go to a-grid
              else
                 go to a-tm-tml
              end-if
           end-if
           if f2                go to a-tm-tml.

           if f8
              initialize util-scrtml
              move "SCRTML"        to scrtml-called
              move k-program-id    to scrtml-caller
              call "SCRTML"     using stringhe util-scrtml
              cancel "SCRTML"
              if scrtml-out-cod = spaces
                 go to a-tm-tml
               else
                 move scrtml-out-cod  to tm-tml (i)
              end-if
           end-if

           perform x-controlla-tm-tml
           if x-f3-ok-parziale = "N"
              move x-f3-msg              to wb-msg
              perform box-msg
              go to a-tm-tml
           end-if
           if tm-des (i)     = spaces and
              tm-tml (i) not = spaces
              move ftm-des         to tm-des (i)
              move ftm-tip         to tm-tip (i)
              move ftm-size        to tm-size (i)
              move ftm-size-dec    to tm-size-dec (i)
              move ftm-h-size      to tm-h-size (i)
              move ftm-v-size      to tm-v-size (i)
              move ftm-label       to tm-label (i)
              move ftm-label-h-pos-rel to tm-label-h-pos-rel (i)
              move ftm-label-v-pos-rel to tm-label-v-pos-rel (i)
              move ftm-label-h-size   to tm-label-h-size (i)
              move ftm-label-v-size   to tm-label-v-size (i)
              move ftm-fl-grid-frame  to tm-fl-grid-frame (i)
              move ftm-fl-hnd-label   to tm-fl-hnd-label (i)
              move ftm-case        to tm-case (i)
              move ftm-frame-style to tm-frame-style (i)
              move ftm-fl-full-height to tm-fl-full-height (i)
              move ftm-fl-notify   to tm-fl-notify (i)
              move ftm-fl-cent-head   to tm-fl-cent-head (i)
              move ftm-check-true  to tm-check-true (i)
              move ftm-check-false to tm-check-false (i)
              move ftm-lab-def     to tm-lab-def (i)
           end-if
           perform z-carica-controlli
           display s-mm-1-tab-01
           .
      *
      **---------------------------------------------------------------*
      ** Descrizione template
      **---------------------------------------------------------------*
       a-tm-tml-des.
           if x-enabled-tm-tml-des = 0
              if f2
                 go to a-tm-tml
               else
                 go to a-tm-des
              end-if
           end-if
           move "Descrizione da attribuire al template" to sb-msg
           perform x-status-bar-msg
           accept e-tm-tml-des     on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-tml.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-tml-des
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-tml-des
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Descrizione controllo
      **---------------------------------------------------------------*
       a-tm-des.
           move "Descrizione da porre sul controllo" to sb-msg
           perform x-status-bar-msg
           accept e-tm-des         on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-tml-des.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-des
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-des
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Tipo controllo
      **---------------------------------------------------------------*
       a-tm-tip.
           move "Indicare il tipo di controllo" to sb-msg
           perform x-status-bar-msg
           initialize util-s60
           perform z-s60-tm-tip
           perform z-cambia-controlli
           if f-event
              go to x-test-mouse
           end-if
           if a-operazione = "I"
              evaluate tm-tip (i)
               when k-ctr-frame
                 move "N"             to tm-fl-value (i)
                 modify e-tm-fl-value, value = tm-fl-value (i)
               when k-ctr-grid
                 move "N"             to tm-fl-value (i)
                 modify e-tm-fl-value, value = tm-fl-value (i)
                 move "S"             to tm-fl-grid-dinamica (i)
                 perform z-carica-s95
               when k-ctr-grid-paged
                 move "N"             to tm-fl-value (i)
                 modify e-tm-fl-value, value = tm-fl-value (i)
               when k-ctr-check
                 move "N"             to tm-fl-value (i)
                 modify e-tm-fl-value, value = tm-fl-value (i)
               when k-ctr-merce
                 move "S"             to tm-fl-entry-point (i)
                 perform z-carica-s95
              end-evaluate
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-des.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-tip
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-tip
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Inizio sezione?
      **---------------------------------------------------------------*
       a-tm-pb-bitmap.
           if x-enabled-tm-pb-bitmap = 0
              if f2
                 go to a-tm-tip
               else
                 go to a-tm-id
              end-if
           end-if
           move "Push-button con bitmap?" to sb-msg
           perform x-status-bar-msg
           initialize util-s95
           perform z-s95-tm-pb-bitmap
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-tip.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-pb-bitmap
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-pb-bitmap
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** ID del controllo
      ** Se non definito, lo propongo come l'id maggiore + 1
      **---------------------------------------------------------------*
       a-tm-id.
           if x-enabled-tm-id = 0
              if f2
                 go to a-tm-pb-bitmap
               else
                 go to a-tm-nome
              end-if
           end-if
           if tm-id (i)        = zero and
              a-operazione not = "M"
              compute tm-id (i) = tm-id-max + 1 
              modify e-tm-id, value = tm-id (i)
           end-if
           move "ID del controllo" to sb-msg
           perform x-status-bar-msg
           accept e-tm-id      on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-pb-bitmap.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-id
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-id
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Nome controllo
      **---------------------------------------------------------------*
       a-tm-nome.
           if x-enabled-tm-nome = 0
              if f2
                 go to a-tm-id
               else
                 go to a-tm-fl-value
              end-if
           end-if
           move "Nome controllo" to sb-msg
           perform x-status-bar-msg
           accept e-tm-nome      on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-id.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-nome
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-nome
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Attributo "value"?
      **---------------------------------------------------------------*
       a-tm-fl-value.
           if x-enabled-tm-fl-value = 0
              if f2
                 go to a-tm-nome
               else
                 go to a-tm-indice
              end-if
           end-if
           move "Aggiungo l'attributo 'value' al controllo?" to sb-msg
           perform x-status-bar-msg
           initialize util-s60
           perform z-s60-tm-fl-value
           perform z-cambia-controlli
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-nome.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-fl-value
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-fl-value
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Indice associato al controllo
      **---------------------------------------------------------------*
       a-tm-indice.
           if x-enabled-tm-indice = 0
              if f2
                 go to a-tm-fl-value
               else
                 go to a-tm-liv
              end-if
           end-if
           move "Indice associato al controllo" to sb-msg
           perform x-status-bar-msg
           accept e-tm-indice      on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-fl-value.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-indice
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-indice
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Descrizione controllo
      **---------------------------------------------------------------*
       a-tm-liv.
           move "Livello di screen section" to sb-msg
           perform x-status-bar-msg
           if tm-liv (i) = spaces
              if i-rif = 0
                 if tm-pag = 0
                    move "02"      to tm-liv (i)
                  else
                    move "03"      to tm-liv (i)
                 end-if
               else
                 move tm-liv (i-rif)  to tm-liv (i)
              end-if
              modify e-tm-liv, value = tm-liv (i)
           end-if
           accept e-tm-liv         on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-indice.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-liv
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-liv
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Inizio sezione?
      **---------------------------------------------------------------*
       a-tm-fl-sezione.
           if x-enabled-tm-fl-sezione = 0
              if f2
                 go to a-tm-liv
               else
                 go to a-tm-exception
              end-if
           end-if
           move "Inizio una nuova sezione?" to sb-msg
           perform x-status-bar-msg
           initialize util-s95
           perform z-s95-tm-fl-sezione
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-liv.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-fl-sezione
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-fl-sezione
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Exception-value (per bottoni)
      **---------------------------------------------------------------*
       a-tm-exception.
           if x-enabled-tm-exception = 0
              if f2
                 go to a-tm-fl-sezione
               else
                 go to a-tm-status-bar
              end-if
           end-if
           move "Contenuto dell'attributo 'exception-value'" to sb-msg
           perform x-status-bar-msg
           accept e-tm-exception      on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-fl-sezione.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-exception
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-exception
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Layout-data
      **---------------------------------------------------------------*
       a-tm-status-bar.
           if x-enabled-tm-status-bar = 0
              if f2
                 go to a-tm-exception
               else
                 go to a-tm-fl-notify
              end-if
           end-if
           move "Contenuto della status bar" to sb-msg
           perform x-status-bar-msg
           accept e-tm-status-bar  on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-exception.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-status-bar
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-status-bar
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Attributo "notify-selchange"?
      **---------------------------------------------------------------*
       a-tm-fl-notify.
           if x-enabled-tm-fl-notify = 0
              if f2
                 go to a-tm-status-bar
               else
                 go to a-tm-fl-self-act
              end-if
           end-if
           move "Attivo l'attributo 'notify-selchange?'" to sb-msg
           perform x-status-bar-msg
           initialize util-s95
           perform z-s95-tm-fl-notify
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-status-bar.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-fl-notify
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-fl-notify
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Attributo "self-act"?
      **---------------------------------------------------------------*
       a-tm-fl-self-act.
           if x-enabled-tm-fl-self-act = 0
              if f2
                 go to a-tm-fl-notify
               else
                 go to a-tm-fl-entry-point
              end-if
           end-if
           move "Attivo l'attributo 'self-act-selchange?'" to sb-msg
           perform x-status-bar-msg
           initialize util-s95
           perform z-s95-tm-fl-self-act
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-fl-notify.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-fl-self-act
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-fl-self-act
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Definisco un entry point in gestione controllo?
      **---------------------------------------------------------------*
       a-tm-fl-entry-point.
           if x-enabled-tm-fl-entry-point = 0
              if f2
                 go to a-tm-fl-self-act
               else
                 go to a-tm-check-true
              end-if
           end-if
           move "Definisco un entry point prima della 'call' alla funzio
      -         "ne?"              to sb-msg
           perform x-status-bar-msg
           initialize util-s95
           perform z-s95-tm-fl-entry-point
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-fl-self-act.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-fl-entry-point
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-fl-entry-point
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Valori del check
      **---------------------------------------------------------------*
       a-tm-check-true.
           if x-enabled-tm-check-true = 0
              if f2
                 go to a-tm-fl-entry-point
               else
                 go to a-tm-check-false
              end-if
           end-if
           move "Valori 'True' e 'False' del check-box" to sb-msg
           perform x-status-bar-msg
           accept e-tm-check-true      on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-fl-entry-point.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-check-true
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-check-true
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Valori del check
      **---------------------------------------------------------------*
       a-tm-check-false.
           if x-enabled-tm-check-false = 0
              if f2
                 go to a-tm-check-true
               else
                 go to a-fine-pagina-01
              end-if
           end-if
           move "Valori 'True' e 'False' del check-box" to sb-msg
           perform x-status-bar-msg
           accept e-tm-check-false      on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-check-true.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-check-false
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-check-false
           end-if
           .

      **---------------------------------------------------------------**
      ** Fine sezione di accept
      **---------------------------------------------------------------**
       a-fine-pagina-01.
           .
           modify e-tab-control, value = 2
           set event-type                to cmd-tabchanged
           set event-data-1              to 2
           perform z-99-event-form
           .

       a-pagina-02.
           move 2                        to w-tab-current-page-test

           perform z-carica-vari
           perform z-forza-cambia-pagina-02
           initialize event-type
           .
      *
      **---------------------------------------------------------------*
      ** Dimensione in caratteri del controllo
      **---------------------------------------------------------------*
       a-tm-size.
           if x-enabled-tm-size = 0
              if f2
                 modify e-tab-control, value = 1
                 set event-type             to cmd-tabchanged
                 set event-data-1           to 1
                 perform z-99-event-form-1
                 perform z-cambia-controlli
                 go to a-tm-check-false
               else
                 go to a-tm-size-dec
              end-if
           end-if
           move "Lunghezza in caratteri, da 1 a 8000" to sb-msg
           perform x-status-bar-msg
           accept e-tm-size         on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2
              modify e-tab-control, value = 1
              set event-type             to cmd-tabchanged
              set event-data-1           to 1
              perform z-99-event-form-1
              perform z-cambia-controlli
              go to a-tm-check-false
           end-if

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-size
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-size
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Numero di decimali
      **---------------------------------------------------------------*
       a-tm-size-dec.
           if x-enabled-tm-size-dec = 0
              if f2
                 go to a-tm-size
               else
                 go to a-tm-v-size
              end-if
           end-if
           move "Numero di decimali, da 0 a 6" to sb-msg
           perform x-status-bar-msg
           accept e-tm-size-dec         on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-size.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-size-dec
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-size-dec
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Dimensione in caratteri del controllo
      **---------------------------------------------------------------*
       a-tm-v-size.
           if x-enabled-tm-v-size = 0
              if f2
                 go to a-tm-size-dec
               else
                 go to a-tm-h-size     
              end-if
           end-if
           if tm-v-size (i) = zero
              evaluate tm-tip (i)
               when k-ctr-combo
                 move 5            to tm-v-size (i)
              end-evaluate
              modify e-tm-v-size, value = tm-v-size (i)
           end-if
           move "Altezza in linee del controllo" to sb-msg
           perform x-status-bar-msg
           initialize util-u20
           perform z-u20-tm-v-size
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-size-dec.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-v-size
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-v-size
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Dimensione in caratteri del controllo
      **---------------------------------------------------------------*
       a-tm-h-size.
           if x-enabled-tm-h-size = 0
              if f2
                 go to a-tm-v-size
               else
                 go to a-tm-v-pos      
              end-if
           end-if
           if tm-h-size (i) = zero
              evaluate tm-tip (i)
               when k-ctr-numero
                 if tm-size-dec (i) = zero
                    compute tm-h-size (i) = tm-size (i) + 1
                  else
                    compute tm-h-size (i) = tm-size (i) + 2
                 end-if
               when k-ctr-valore
                 compute tm-h-size (i) = tm-size (i) + 1
               when k-ctr-data
                 move 10           to tm-h-size (i)
               when k-ctr-ora
                 move 5            to tm-h-size (i)
               when k-ctr-alfanumerico
                 move tm-size (i)       to tm-h-size (i)
              end-evaluate
              modify e-tm-h-size, value = tm-h-size (i)
           end-if
           move "Larghezza in caratteri del controllo" to sb-msg
           perform x-status-bar-msg
           initialize util-u20
           perform z-u20-tm-h-size
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-v-size.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-h-size
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-h-size
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Posizione verticale del controllo
      **---------------------------------------------------------------*
       a-tm-v-pos.
           if x-enabled-tm-v-pos = 0
              if f2
                 go to a-tm-h-size
               else
                 go to a-tm-h-pos     
              end-if
           end-if
           if tm-v-pos (i) = 0
              if i-rif = 0
                 move 2,2          to tm-v-pos (i)
                 move 2            to tm-h-pos (i)
               else
                 compute tm-h-pos-fine-max = 
                    tm-h-pos (i-rif) + tm-h-size (i-rif) + 0,5 
                 if (tm-h-pos-fine-max + tm-h-size (i)) > tm-frm-h-size
                    compute tm-v-pos (i) = tm-v-pos (i-rif) + 2,2
                    move 2         to tm-h-pos (i)
                  else
                    move tm-v-pos (i-rif) to tm-v-pos (i)
                    move tm-h-pos-fine-max to tm-h-pos (i)
                 end-if
              end-if
              if tm-tip (i) = k-ctr-tab-control
                 move 1,5          to tm-h-pos (i)
              end-if
              modify e-tm-v-pos, value = tm-v-pos (i)
              modify e-tm-h-pos, value = tm-h-pos (i)
           end-if
           move "Posizione verticale del controllo" to sb-msg
           perform x-status-bar-msg
           initialize util-u20
           perform z-u20-tm-v-pos
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-h-size.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-v-pos
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-v-pos
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Posizione orizzontale del controllo
      ** Se non definita, la propongo posizionandomi a dx dell'ultimo 
      **  controllo inserito, se sono sulla stessa riga
      **---------------------------------------------------------------*
       a-tm-h-pos.
           if x-enabled-tm-h-pos = 0
              if f2
                 go to a-tm-v-pos
               else
                 go to a-tm-align
              end-if
           end-if
           move "Posizione orizzontale del controllo" to sb-msg
           perform x-status-bar-msg
           initialize util-u20
           perform z-u20-tm-h-pos
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-v-pos.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-h-pos
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-h-pos
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Attributo "align"
      **---------------------------------------------------------------*
       a-tm-align.
           if x-enabled-tm-align = 0
              if f2
                 go to a-tm-h-pos
               else
                 go to a-tm-case
              end-if
           end-if
           if tm-align (i) = spaces
              evaluate tm-tip (i) 
               when "N"
                 move "R"                to tm-align (i)
               when other
                 move "L"                to tm-align (i)
              end-evaluate
              display e-tm-align
           end-if
           move "Impostazione attributo 'alignment'" to sb-msg
           perform x-status-bar-msg
           initialize util-s60
           perform z-s60-tm-align
           perform z-cambia-controlli
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-h-pos.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-align
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-align
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Attributo "case"
      **---------------------------------------------------------------*
       a-tm-case.
           if x-enabled-tm-case = 0
              if f2
                 go to a-tm-align
               else
                 go to a-tm-color-control
              end-if
           end-if
           if tm-case  (i) = spaces
              move "I"                   to tm-case (i)
              display e-tm-case 
           end-if
           move "Modalita` di conversione 'case' lettere" to sb-msg
           perform x-status-bar-msg
           initialize util-s60
           perform z-s60-tm-case 
           perform z-cambia-controlli
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-align.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-case 
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-case 
           end-if
           .
      *
      *---------------------------------------------------------------*
      * Colore del controllo
      *---------------------------------------------------------------*
       a-tm-color-control.
           if x-enabled-tm-color-control = 0
              if f2
                 go to a-tm-case
               else
                 go to a-tm-enabled      
              end-if
           end-if

           move "Colore del controllo" to sb-msg
           perform x-status-bar-msg

           perform x-attiva-f8-aiuto

           accept e-tm-color-control on exception continue.
           
           perform x-disattiva-f8-aiuto

           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-case.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           if f8
              initialize util-gcol
              move tm-color-control (i)  to gcol-color
              call "GETCOLOR"         using stringhe util-gcol
              cancel "GETCOLOR"
              move gcol-color            to tm-color-control (i)
           end-if

           perform x-controlla-tm-color-control
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-color-control
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Attributo "enabled"
      ** Se vale "V", allora propongo "S" nell'handle della label
      **---------------------------------------------------------------*
       a-tm-enabled.
           if x-enabled-tm-enabled = 0
              if f2
                 go to a-tm-color-control
               else
                 go to a-tm-cond-enabled
              end-if
           end-if
           move "Impostazione attributo 'enabled'" to sb-msg
           perform x-status-bar-msg
           move tm-enabled (i)     to tm-enabled-salva
           initialize util-s60
           perform z-s60-tm-enabled
           if tm-enabled (i) not = tm-enabled-salva
              if tm-enabled (i) = "V"
                 move "S"          to tm-fl-hnd-label (i)
                 initialize util-s95
                 move "Riempi"     to s95-ope
                 perform z-s95-tm-fl-hnd-label
              end-if
           end-if
           perform z-cambia-controlli
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-color-control.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-enabled
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-enabled
           end-if
           .
      **---------------------------------------------------------------**
      ** Accept di: Condizione enabled=1
      **---------------------------------------------------------------**
       a-tm-cond-enabled.
           if x-enabled-tm-cond-enabled = 0
              if f2
                 go to a-tm-enabled
               else
                 go to a-tm-visible
              end-if
           end-if
           move "Condizione necessarie per abilitare il campo" to sb-msg
           perform x-status-bar-msg
           accept e-tm-cond-enabled on exception continue.
           if tm-cond-enabled (i) (1:2) <> "if"
      *    if tm-cond-enabled (i) (1:2) <> "if"        and
      *       tm-cond-enabled (i) (1:2) not alphabetic
              move "N"                   to tm-enabled-auto (i)
            else
              move "S"                   to tm-enabled-auto (i)
           end-if
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2
              go to a-tm-enabled
           end-if
           if f3
              go to a-riga-dettaglio-fine
           end-if
 
           perform x-controlla-tm-cond-enabled
           if x-f3-ok-parziale = "N"
              move x-f3-msg to wb-msg
              perform vbx-msg-error
              go to a-tm-cond-enabled
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Attributo "visible"
      **---------------------------------------------------------------*
       a-tm-visible.
           if x-enabled-tm-visible = 0
              if f2
                 go to a-tm-cond-enabled
               else
                 go to a-tm-cond-visible
              end-if
           end-if
           move "Impostazione attributo 'visible'" to sb-msg
           perform x-status-bar-msg
           initialize util-s60
           perform z-s60-tm-visible
           perform z-cambia-controlli
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-cond-enabled.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-visible
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-visible
           end-if
           .
      **---------------------------------------------------------------**
      ** Accept di: Condizione visible=1
      **---------------------------------------------------------------**
       a-tm-cond-visible.
           if x-enabled-tm-cond-visible = 0
              if f2
                 go to a-tm-visible
               else
                 go to a-tm-layout
              end-if
           end-if
           move "Condizione necessaria per visualizzare il campo" 
                                   to sb-msg
           perform x-status-bar-msg
           accept e-tm-cond-visible on exception continue.
           if tm-cond-visible (i) (1:2) not = "if"
              move "N"                   to tm-visible-auto (i)
            else
              move "S"                   to tm-visible-auto (i)
           end-if
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2
              go to a-tm-visible
           end-if
           if f3
              go to a-riga-dettaglio-fine
           end-if
 
           perform x-controlla-tm-cond-visible
           if x-f3-ok-parziale = "N"
              move x-f3-msg to wb-msg
              perform vbx-msg-error
              go to a-tm-cond-visible
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Layout-data
      **---------------------------------------------------------------*
       a-tm-layout.
           if x-enabled-tm-layout = 0
              if f2
                 go to a-tm-cond-visible
               else
                 go to a-tm-fl-color-form
              end-if
           end-if
           move "Contenuto dell'attributo 'layout-data'" to sb-msg
           perform x-status-bar-msg
           accept e-tm-layout      on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-cond-visible.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-layout
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-layout
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Forzo "ext-color-controls"?
      **---------------------------------------------------------------*
       a-tm-fl-color-form.
           if x-enabled-tm-fl-color-form = 0
              if f2
                 go to a-tm-layout
               else
                 go to a-tm-fl-evidenza
              end-if
           end-if
           move "Forzo 'color ext-color-controls'?" to sb-msg
           perform x-status-bar-msg
           initialize util-s95
           perform z-s95-tm-fl-color-form
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-layout.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-fl-color-form
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-fl-color-form
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Attributo "font in evidenza"?
      **---------------------------------------------------------------*
       a-tm-fl-evidenza.
           if x-enabled-tm-fl-evidenza = 0
              if f2
                 go to a-tm-fl-color-form
               else
                 go to a-tm-fl-secure
              end-if
           end-if
 
           perform x-aiuto-tm-fl-evidenza

           initialize util-s95
           perform z-s95-tm-fl-evidenza

           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-fl-color-form.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-fl-evidenza
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-fl-evidenza
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Attributo "secure"?
      **---------------------------------------------------------------*
       a-tm-fl-secure.
           if x-enabled-tm-fl-secure = 0
              if f2
                 go to a-tm-fl-evidenza
               else
                 go to a-tm-css-classe
              end-if
           end-if
 
           perform x-aiuto-tm-fl-secure

           initialize util-s95
           perform z-s95-tm-fl-secure

           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-fl-evidenza.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-fl-secure
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-fl-secure
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Classe CSS
      **---------------------------------------------------------------*
       a-tm-css-classe.
           if x-enabled-tm-css-classe = 0
              if f2
                 go to a-tm-fl-secure
               else
                 go to a-fine-pagina-02
              end-if
           end-if
           move "Classe CSS particolare da usare" to sb-msg
           perform x-status-bar-msg
           accept e-tm-css-classe      on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-fl-secure.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-css-classe
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-css-classe
           end-if
           .

      **---------------------------------------------------------------**
      ** Fine sezione di accept
      **---------------------------------------------------------------**
       a-fine-pagina-02.
           .
           modify e-tab-control, value = 3
           set event-type                to cmd-tabchanged
           set event-data-1              to 3
           perform z-99-event-form
           .

       a-pagina-03.
           move 3                        to w-tab-current-page-test

           perform z-carica-vari
           perform z-forza-cambia-pagina-03
           initialize event-type
           .
      *
      **---------------------------------------------------------------*
      ** Nome label generica
      **---------------------------------------------------------------*
       a-tm-lab-def.
           if x-enabled-tm-lab-def = 0
              if f2
                 modify e-tab-control, value = 2
                 set event-type             to cmd-tabchanged
                 set event-data-1           to 2
                 perform z-99-event-form-1
                 perform z-cambia-controlli
                 go to a-tm-css-classe
               else
                 go to a-tm-label      
              end-if
           end-if
           move "Nome label generica" to sb-msg
           perform x-status-bar-msg
           perform x-attiva-f8-aiuto
           accept e-tm-lab-def      on exception continue.
           perform x-disattiva-f8-aiuto
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2
              modify e-tab-control, value = 2
              set event-type             to cmd-tabchanged
              set event-data-1           to 2
              perform z-99-event-form-1
              perform z-cambia-controlli
              go to a-tm-css-classe
           end-if

           if f3
              go to a-riga-dettaglio-fine
           end-if

           if f8
              initialize util-scrmge
              move k-scr-msg-label to scrmge-tip 
              call "SCRMGE"     using stringhe util-scrmge
              cancel "SCRMGE"     
              if scrmge-id = spaces
                 go to a-tm-lab-def
               else
                 move scrmge-id    to tm-lab-def (i)
                 display e-tm-lab-def 
              end-if
           end-if

           perform x-controlla-tm-lab-def
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-lab-def
           end-if

      **---------------------------------------------------------------*
      ** Se sono in inserimento e cerco una label generica, propongo il 
      **   suo contenuto nella label del programma
      **---------------------------------------------------------------*
           if tm-lab-def (i) not = spaces
              initialize pgs-rec
              move spaces             to pgs-pac
              move spaces             to pgs-prg
              move k-scr-msg-label    to pgs-tip   
              move tm-lab-def (i)     to pgs-id   
              move 0                  to pgs-sez   
              perform rd-scrpgmsg
              if w-verbo-ok
                 move pgs-msg         to tm-label (i)
                 display e-tm-label
              end-if
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Etichetta
      **---------------------------------------------------------------*
       a-tm-label.
           if x-enabled-tm-label = 0
              if f2
                 go to a-tm-lab-def
               else
                 go to a-tm-label-v-pos-rel
              end-if
           end-if
           if a-operazione = "I"
              if tm-label (i) = spaces
                 move tm-des (i)  to tm-label (i)
                 call "c$toupper" using tm-label (i), value 1
                 modify e-tm-label, value = tm-label (i)
              end-if
              if tm-tml (i)     not = spaces     and
                 tm-tml-des (i) not = tm-des (i)
                 move tm-des (i)  to tm-label (i)
                 call "c$toupper" using tm-label (i), value 1
                 modify e-tm-label, value = tm-label (i)
              end-if
           end-if
           move "Etichetta relativa al controllo" to sb-msg
           perform x-status-bar-msg
           accept e-tm-label      on exception continue.
           perform z-cambia-controlli
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-lab-def.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-label
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-label
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Posizione verticale etichetta (relativa)
      **---------------------------------------------------------------*
       a-tm-label-v-pos-rel.
           if x-enabled-tm-label-v-pos-rel = 0
              if f2
                 go to a-tm-label
               else
                 go to a-tm-label-h-pos-rel
              end-if
           end-if
           if a-operazione = "I"
              if tm-label-v-pos-rel (i) = zero
                 move -1,2        to tm-label-v-pos-rel (i)
                 modify e-tm-label-v-pos-rel, 
                    value = tm-label-v-pos-rel (i)
              end-if
           end-if
           move "Posizione verticale etichetta relativa al controllo" 
                                   to sb-msg
           perform x-status-bar-msg
           initialize util-u20
           perform z-u20-tm-label-v-pos-rel
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-label.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-label-v-pos-rel
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-label-v-pos-rel
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Posizione orizzontale etichetta (relativa)
      **---------------------------------------------------------------*
       a-tm-label-h-pos-rel.
           if x-enabled-tm-label-h-pos-rel = 0
              if f2
                 go to a-tm-label-v-pos-rel
               else
                 go to a-tm-label-v-size
              end-if
           end-if
           if a-operazione = "I"
              if tm-label-h-pos-rel (i) = zero
                 move 0           to tm-label-h-pos-rel (i)
                 modify e-tm-label-h-pos-rel, 
                    value = tm-label-h-pos-rel (i)
              end-if
           end-if
           move "Posizione orizzontale etichetta relativa al controllo" 
                                   to sb-msg
           perform x-status-bar-msg
           initialize util-u20
           perform z-u20-tm-label-h-pos-rel
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-label.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-label-h-pos-rel
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-label-h-pos-rel
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Dimensione verticale etichetta
      **---------------------------------------------------------------*
       a-tm-label-v-size.
           if x-enabled-tm-label-v-size = 0
              if f2
                 go to a-tm-label-h-pos-rel
               else
                 go to a-tm-label-h-size    
              end-if
           end-if
           if a-operazione = "I"
              if tm-label-v-size (i) = zero
                 move 0           to tm-label-v-size (i)
                 modify e-tm-label-v-size, 
                    value = tm-label-v-size (i)
              end-if
           end-if
           move "Dimensione verticale etichetta" to sb-msg
           perform x-status-bar-msg
           initialize util-u20
           perform z-u20-tm-label-v-size
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-label-h-pos-rel.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-label-v-size
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-label-v-size
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Dimensione orizzontale etichetta
      **---------------------------------------------------------------*
       a-tm-label-h-size.
           if x-enabled-tm-label-h-size = 0
              if f2
                 go to a-tm-label-v-size
               else
                 go to a-tm-fl-hnd-label
              end-if
           end-if
           move "Dimensione orizzontale etichetta" to sb-msg
           perform x-status-bar-msg
           initialize util-u20
           perform z-u20-tm-label-h-size
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-label-v-size.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-label-h-size
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-label-h-size
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** La label ha l'handle?
      **---------------------------------------------------------------*
       a-tm-fl-hnd-label.
           if x-enabled-tm-fl-hnd-label = 0
              if f2
                 go to a-tm-label-h-size
               else
                 go to a-tm-fl-val-label
              end-if
           end-if
           move "La label deve avere l'handle?" to sb-msg
           perform x-status-bar-msg
           initialize util-s95
           perform z-s95-tm-fl-hnd-label
           perform z-cambia-controlli
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-label-h-size.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-fl-hnd-label
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-fl-hnd-label
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** La label ha il "value"?
      ** Se si deve avere anche l'handle!!
      **---------------------------------------------------------------*
       a-tm-fl-val-label.
           if x-enabled-tm-fl-val-label = 0
              if f2
                 go to a-tm-fl-hnd-label
               else
                 go to a-tm-color-label
              end-if
           end-if
           move "La label deve avere l'attributo 'value'?" to sb-msg
           perform x-status-bar-msg
           initialize util-s95
           perform z-s95-tm-fl-val-label
      *    if tm-fl-val-label (i) = "S"
      *       move "S"             to tm-fl-hnd-label (i)
      *       initialize util-s95
      *       move "Riempi"        to s95-ope
      *       perform z-s95-tm-fl-hnd-label
      *       display e-tm-fl-hnd-label
      *    end-if
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-fl-hnd-label.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-fl-val-label
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-fl-val-label
           end-if
           .
      *
      *---------------------------------------------------------------*
      * Colore dell'etichetta del controllo
      *---------------------------------------------------------------*
       a-tm-color-label.
           if x-enabled-tm-color-label = 0
              if f2
                 go to a-tm-fl-val-label
               else
                 go to a-tm-s67-liv-ric      
              end-if
           end-if

           move "Colore dell'etichetta" to sb-msg
           perform x-status-bar-msg

           perform x-attiva-f8-aiuto

           accept e-tm-color-label on exception continue.
           
           perform x-disattiva-f8-aiuto

           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-fl-val-label.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           if f8
              initialize util-gcol
              move tm-color-label (i)  to gcol-color
              call "GETCOLOR"         using stringhe util-gcol
              cancel "GETCOLOR"
              move gcol-color            to tm-color-label (i)
           end-if

           perform x-controlla-tm-color-label
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-color-label
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Livello ricerca (x COGS67)
      **---------------------------------------------------------------*
       a-tm-s67-liv-ric.
           if x-enabled-tm-s67-liv-ric = 0
              if f2
                 go to a-tm-color-label
               else
                 go to a-tm-u10-divisa
              end-if
           end-if
           move "Livello di ricerca (S67-LIV-RICERCA)" to sb-msg
           perform x-status-bar-msg
           accept e-tm-s67-liv-ric      on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-color-label.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-s67-liv-ric
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-s67-liv-ric
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** LNome campo divisa x COGU10
      **---------------------------------------------------------------*
       a-tm-u10-divisa.
           if x-enabled-tm-u10-divisa  = 0
              if f2
                 go to a-tm-s67-liv-ric
               else
                 go to a-tm-u10-data
              end-if
           end-if
           move "Nome campo divisa (U10-DIVISA)" to sb-msg
           perform x-status-bar-msg
           accept e-tm-u10-divisa       on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-s67-liv-ric.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-u10-divisa 
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-u10-divisa 
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Nome campo data x COGU10
      **---------------------------------------------------------------*
       a-tm-u10-data.
           if x-enabled-tm-u10-data = 0
              if f2
                 go to a-tm-u10-divisa
               else
                 go to a-tm-u10-tipo-dato
              end-if
           end-if
           move "Nome campo data (U10-DATA)" to sb-msg
           perform x-status-bar-msg
           accept e-tm-u10-data      on exception continue.
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-u10-divisa.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-u10-data
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-u10-data
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** U10-TIPO-DATO
      **---------------------------------------------------------------*
       a-tm-u10-tipo-dato.
           if x-enabled-tm-u10-tipo-dato  = 0
              if f2
                 go to a-tm-u10-data
               else
                 go to a-tm-s52-verifica
              end-if
           end-if
           if tm-u10-tipo-dato  (i) = spaces
              move "I"             to tm-u10-tipo-dato (i)
              display e-tm-u10-tipo-dato 
           end-if
           move "U10-TIPO-DATO"    to sb-msg
           perform x-status-bar-msg
           initialize util-s60
           perform z-s60-tm-u10-tipo-dato 
           perform z-cambia-controlli
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-u10-data.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-u10-tipo-dato 
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-u10-tipo-dato 
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** S52-VERIFICA
      **---------------------------------------------------------------*
       a-tm-s52-verifica.
           if x-enabled-tm-s52-verifica  = 0
              if f2
                 go to a-tm-u10-tipo-dato
               else
                 go to a-tm-s93-verifica
              end-if
           end-if
           if tm-s52-verifica  (i) = spaces
              move "1"             to tm-s52-verifica (i)
              display e-tm-s52-verifica 
           end-if
           evaluate tm-s52-verifica (i)
            when "1"
              move "Data completa, verifico tutto" to sb-msg
            when "2"
              move "Data completa, verifico tutto eccetto data=0" 
                                   to sb-msg
            when "3"
              move "Data completa, nessuna verifica" to sb-msg
            when "4"
              move "Solo anno, verifica" to sb-msg
            when "5"
              move "Solo anno, verifico tutto eccetto anno=0" to sb-msg
            when "6"
              move "Mese/anno, verifico tutto" to sb-msg
            when "7"
              move "Mese/anno, verifico tutto eccetto = 0" to sb-msg
            when "8"
              move "Giorno/mese, verifico tutto" to sb-msg
            when "9"
              move "Giorno/mese, verifico tutto eccetto = 0" to sb-msg
           end-evaluate
           perform x-status-bar-msg
           initialize util-s60
           perform z-s60-tm-s52-verifica 
           perform z-cambia-controlli
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-u10-tipo-dato.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-s52-verifica 
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-s52-verifica 
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** S93-VERIFICA
      **---------------------------------------------------------------*
       a-tm-s93-verifica.
           if x-enabled-tm-s93-verifica  = 0
              if f2
                 go to a-tm-s52-verifica
               else
                 go to a-tm-u10-edit-punti
              end-if
           end-if
           if tm-s93-verifica  (i) = spaces
              move "1"             to tm-s93-verifica (i)
              display e-tm-s93-verifica 
           end-if
           evaluate tm-s93-verifica (i)
            when "1"
              move "Ora completa, verifico tutto" to sb-msg
            when "2"
              move "Ora completa, verifico tutto eccetto data=0" 
                                   to sb-msg
            when "3"
              move "Ora completa, nessuna verifica" to sb-msg
           end-evaluate
           perform x-status-bar-msg
           initialize util-s60
           perform z-s60-tm-s93-verifica 
           perform z-cambia-controlli
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-s52-verifica.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-s93-verifica 
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-s93-verifica 
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Attributo "value"?
      **---------------------------------------------------------------*
       a-tm-u10-edit-punti.
           if x-enabled-tm-u10-edit-punti = 0
              if f2
                 go to a-tm-s93-verifica
               else
                 go to a-fine-pagina-03
              end-if
           end-if
           move "Aggiungo l'attributo 'value' al controllo?" to sb-msg
           perform x-status-bar-msg
           initialize util-s95
           perform z-s95-tm-u10-edit-punti
           perform z-cambia-controlli
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-s93-verifica.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-u10-edit-punti
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-u10-edit-punti
           end-if
           .

      **---------------------------------------------------------------**
      ** Fine sezione di accept
      **---------------------------------------------------------------**
       a-fine-pagina-03.
           .
           modify e-tab-control, value = 4
           set event-type                to cmd-tabchanged
           set event-data-1              to 4
           perform z-99-event-form
           .

       a-pagina-04.
           move 4                        to w-tab-current-page-test

           perform z-carica-vari
           perform z-forza-cambia-pagina-04
           initialize event-type
           .
      *
      **---------------------------------------------------------------*
      ** Definisco un entry point in gestione controllo?
      **---------------------------------------------------------------*
       a-tm-fl-grid-dinamica.
           if x-enabled-tm-fl-grid-dinamica = 0
              if f2
                 modify e-tab-control, value = 3
                 set event-type             to cmd-tabchanged
                 set event-data-1           to 3
                 perform z-99-event-form-1
                 perform z-cambia-controlli
                 go to a-tm-fl-entry-point
               else
                 go to a-tm-fl-pos-man
              end-if
           end-if
           move "La griglia si auto-ridimensiona?" to sb-msg
           perform x-status-bar-msg
           initialize util-s95
           perform z-s95-tm-fl-grid-dinamica
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2
              modify e-tab-control, value = 2
              set event-type             to cmd-tabchanged
              set event-data-1           to 2
              perform z-99-event-form-1
              perform z-cambia-controlli
              go to a-tm-u10-edit-punti
           end-if

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-fl-grid-dinamica
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-fl-grid-dinamica
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Attributo "pos-man"?
      **---------------------------------------------------------------*
       a-tm-fl-pos-man.
           if x-enabled-tm-fl-pos-man = 0
              if f2
                 go to a-tm-fl-grid-dinamica
               else
                 go to a-tm-fl-headings
              end-if
           end-if
 
           perform x-aiuto-tm-fl-pos-man

           initialize util-s95
           perform z-s95-tm-fl-pos-man

           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-fl-grid-dinamica.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-fl-pos-man
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-fl-pos-man
           end-if
           .
      **---------------------------------------------------------------**
      ** Accept di: Opzione 'Headings'
      **---------------------------------------------------------------**
       a-tm-fl-headings.
 
           if x-enabled-tm-fl-headings = 0
              if f2
                 go to a-tm-fl-pos-man
               else
                 go to a-tm-fl-cent-head
              end-if
           end-if
 
           perform x-aiuto-tm-fl-headings
 
           initialize util-s95
           perform z-s95-tm-fl-headings
           modify e-tm-fl-headings, color = ext-color-label
 
           perform z-99-exception-form
           evaluate z-exception-prosegui
            when k-exc-esci
              go to a4
            when k-exc-rimani
              go to a-tm-fl-headings
           end-evaluate
 
           if f-event
              go to x-test-mouse
           end-if
           if f2
              go to a-tm-fl-pos-man
           end-if
           if f3
              go to a-riga-dettaglio-fine
           end-if
 
           perform x-controlla-tm-fl-headings
           if x-f3-ok-parziale = "N"
              move x-f3-msg to wb-msg
              perform vbx-msg-error
              go to a-tm-fl-headings
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Attributo "notify-selchange"?
      **---------------------------------------------------------------*
       a-tm-fl-cent-head.
           if x-enabled-tm-fl-cent-head = 0
              if f2
                 go to a-tm-fl-headings
               else
                 go to a-tm-grid-prf-col
              end-if
           end-if
           if tm-fl-cent-head (i) not = "S" and
              tm-fl-cent-head (i) not = "N"
              move "S"             to tm-fl-cent-head (i)
           end-if
           move "Attivo l'attributo centered headings?'" to sb-msg
           perform x-status-bar-msg
           initialize util-s95
           perform z-s95-tm-fl-cent-head
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-fl-headings.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-fl-cent-head
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-fl-cent-head
           end-if
           .
      **---------------------------------------------------------------**
      ** Accept di: prefisso colonne griglia
      **---------------------------------------------------------------**
       a-tm-grid-prf-col.
 
           if x-enabled-tm-grid-prf-col = 0
              if f2
                 go to a-tm-fl-cent-head
               else
                 go to a-tm-grid-max-row
              end-if
           end-if
 
           perform x-aiuto-tm-grid-prf-col
 
           accept e-tm-grid-prf-col on exception continue.
           modify e-tm-grid-prf-col, color = ext-color-controls
 
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2
              go to a-tm-fl-cent-head
           end-if
           if f3
              go to a-riga-dettaglio-fine
           end-if
 
           perform x-controlla-tm-grid-prf-col
           if x-f3-ok-parziale = "N"
              move x-f3-msg to wb-msg
              perform vbx-msg-error
              go to a-tm-grid-prf-col
           end-if
           .
      **---------------------------------------------------------------**
      ** Accept di: prefisso colonne griglia
      **---------------------------------------------------------------**
       a-tm-grid-max-row.
 
           if x-enabled-tm-grid-max-row = 0
              if f2
                 go to a-tm-grid-prf-col
               else
                 go to a-tm-cond-row-color
              end-if
           end-if
 
           perform x-aiuto-tm-grid-max-row
 
           accept e-tm-grid-max-row on exception continue.
           modify e-tm-grid-max-row, color = ext-color-controls
 
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2
              go to a-tm-grid-prf-col
           end-if
           if f3
              go to a-riga-dettaglio-fine
           end-if
 
           perform x-controlla-tm-grid-max-row
           if x-f3-ok-parziale = "N"
              move x-f3-msg to wb-msg
              perform vbx-msg-error
              go to a-tm-grid-max-row
           end-if
           .
      **---------------------------------------------------------------**
      ** Accept di: Condizione x colore riga griglia
      **---------------------------------------------------------------**
       a-tm-cond-row-color.
 
           if x-enabled-tm-cond-row-color = 0
              if f2
                 go to a-tm-grid-max-row
               else
                 go to a-tm-grid-ctrl-a-ep
              end-if
           end-if
 
           perform x-aiuto-tm-cond-row-color
 
           accept e-tm-cond-row-color on exception continue.
           modify e-tm-cond-row-color, color = ext-color-controls
 
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2
              go to a-tm-grid-max-row
           end-if
           if f3
              go to a-riga-dettaglio-fine
           end-if
 
           perform x-controlla-tm-cond-row-color
           if x-f3-ok-parziale = "N"
              move x-f3-msg to wb-msg
              perform vbx-msg-error
              go to a-tm-cond-row-color
           end-if
           .
      **---------------------------------------------------------------**
      ** Accept di: Entry-point su z-aggiungi-riga ?
      **---------------------------------------------------------------**
       a-tm-grid-ctrl-a-ep.
 
           if x-enabled-tm-grid-ctrl-a-ep = 0
              if f2
                 go to a-tm-cond-row-color
               else
                 go to a-tm-grid-be-ep
              end-if
           end-if
 
           perform x-aiuto-tm-grid-ctrl-a-ep
 
           initialize util-s95
           perform z-s95-tm-grid-ctrl-a-ep
           modify e-tm-grid-ctrl-a-ep, color = ext-color-label
 
           if f-event
              go to x-test-mouse
           end-if
           if f2
              go to a-tm-cond-row-color
           end-if
           if f3
              go to a-riga-dettaglio-fine
           end-if
           .
      **---------------------------------------------------------------**
      ** Accept di: Entry-point su before-event ?
      **---------------------------------------------------------------**
       a-tm-grid-be-ep.
 
           if x-enabled-tm-grid-be-ep = 0
              if f2
                 go to a-tm-grid-ctrl-a-ep
               else
                 go to a-tm-disattiva-tf-ep
              end-if
           end-if
 
           perform x-aiuto-tm-grid-be-ep
 
           initialize util-s95
           perform z-s95-tm-grid-be-ep
           modify e-tm-grid-be-ep, color = ext-color-label
 
           if f-event
              go to x-test-mouse
           end-if
           if f2
              go to a-tm-grid-ctrl-a-ep
           end-if
           if f3
              go to a-riga-dettaglio-fine
           end-if
           .
      **---------------------------------------------------------------**
      ** Accept di: Entry-point su before-event ?
      **---------------------------------------------------------------**
       a-tm-disattiva-tf-ep.
 
           if x-enabled-tm-disattiva-tf-ep = 0
              if f2
                 go to a-tm-grid-be-ep
               else
                 go to a-tm-fl-edit-grid
              end-if
           end-if
 
           perform x-aiuto-tm-disattiva-tf-ep
 
           initialize util-s95
           perform z-s95-tm-disattiva-tf-ep
           modify e-tm-disattiva-tf-ep, color = ext-color-label
 
           if f-event
              go to x-test-mouse
           end-if
           if f2
              go to a-tm-grid-be-ep
           end-if
           if f3
              go to a-riga-dettaglio-fine
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Attributo "value"?
      **---------------------------------------------------------------*
       a-tm-fl-edit-grid.
           if x-enabled-tm-fl-edit-grid = 0
              if f2
                 go to a-tm-disattiva-tf-ep
               else
                 go to a-tm-gor-alt-key
              end-if
           end-if
           move "Modalita' di gestione griglia?" to sb-msg
           perform x-status-bar-msg
           initialize util-s60
           perform z-s60-tm-fl-edit-grid
           perform z-cambia-controlli
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-disattiva-tf-ep.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-fl-edit-grid
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-fl-edit-grid
           end-if
           .
      **---------------------------------------------------------------**
      ** Accept di: Condizione x colore riga griglia
      **---------------------------------------------------------------**
       a-tm-gor-alt-key.
 
           if x-enabled-tm-gor-alt-key = 0
              if f2
                 go to a-tm-fl-edit-grid
               else
                 go to a-tm-frame-style
              end-if
           end-if
 
           perform x-aiuto-tm-gor-alt-key
 
           accept e-tm-gor-alt-key on exception continue.
           modify e-tm-gor-alt-key, color = ext-color-controls
 
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2
              go to a-tm-fl-edit-grid
           end-if
           if f3
              go to a-riga-dettaglio-fine
           end-if
 
           perform x-controlla-tm-gor-alt-key
           if x-f3-ok-parziale = "N"
              move x-f3-msg to wb-msg
              perform vbx-msg-error
              go to a-tm-gor-alt-key
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Attributo "frame style"
      **---------------------------------------------------------------*
       a-tm-frame-style.
           if x-enabled-tm-frame-style  = 0
              if f2
                 go to a-tm-gor-alt-key
               else
                 go to a-tm-fl-grid-frame
              end-if
           end-if
           if tm-frame-style  (i) = spaces
              move "I"                   to tm-frame-style (i)
              display e-tm-frame-style 
           end-if
           move "Modalita` di conversione 'case' lettere" to sb-msg
           perform x-status-bar-msg
           initialize util-s60
           perform z-s60-tm-frame-style 
           perform z-cambia-controlli
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2
              go to a-tm-gor-alt-key
           end-if

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-frame-style 
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-frame-style 
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Attributo "value"?
      **---------------------------------------------------------------*
       a-tm-fl-grid-frame.
           if x-enabled-tm-fl-grid-frame = 0
              if f2
                 go to a-tm-frame-style
               else
                 go to a-tm-fl-full-height
              end-if
           end-if
           move "Frame associato a griglia?" to sb-msg
           perform x-status-bar-msg
           initialize util-s95
           perform z-s95-tm-fl-grid-frame
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-frame-style.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-fl-grid-frame
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-fl-grid-frame
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Attributo "value"?
      **---------------------------------------------------------------*
       a-tm-fl-full-height.
           if x-enabled-tm-fl-full-height = 0
              if f2
                 go to a-tm-fl-grid-frame
               else
                 go to a-riga-dettaglio-fine
              end-if
           end-if
           move "Frame associato a griglia?" to sb-msg
           perform x-status-bar-msg
           initialize util-s95
           perform z-s95-tm-fl-full-height
           if f-event
              go to x-test-mouse
           end-if
           if wesc              go to a-tm-tml-tf.
           if f2                go to a-tm-fl-grid-frame.

           if f3
              go to a-riga-dettaglio-fine
           end-if

           perform x-controlla-tm-fl-full-height
           if x-f3-ok-parziale = "N"
              move x-f3-msg        to wb-msg
              perform box-msg
              go to a-tm-fl-full-height
           end-if
           .

      *******************************************************************
      * Fine della riga di dettaglio                                    *
      * sostituisco la riga sulla griglia e ritorno                     *
      * Se sono in inserimento, aggiungo un record anziche` modificarlo *
      *******************************************************************
       a-riga-dettaglio-fine.
           if tm-tip (i) = "V"
              if tm-sel-idx not = zero
                 move tm-sel-prog  to a4-prog-da
                 move tm-prog (i)  to a4-prog-a
                 perform a4-copia-vf  thru ex-a4
              end-if
              initialize util-scrflv
              move "C"             to scrflv-ctr-tip
              move tm-pac          to scrflv-pac
              move tm-prg          to scrflv-prg
              move tm-frm          to scrflv-frm
              move tm-pag          to scrflv-pag
              move tm-prog (i)     to scrflv-prog
              move tm-des (i)      to scrflv-des
              move tm-nome (i)     to scrflv-nome
              call "SCRFLV"     using stringhe util-scrflv
              cancel "SCRFLV"
              move scrflv-n-val    to tm-n-val (i)
              if scrflv-n-val < 2
                 move "Sono necessari almeno due valori fissi!!" 
                                   to wb-msg
                 perform vbx-msg-error
                 go to a-tm-des
              end-if
           end-if
       
           perform x-imposta-conferma
           if x-f3-ok = "N"
              move x-f3-msg              to wb-msg
              perform vbx-msg

              modify e-tab-control, value = 1
              set event-type             to cmd-tabchanged
              set event-data-1           to 1
              perform z-99-event-form-1
              perform z-cambia-controlli

              go to a-tm-tml
           end-if

           if a-operazione = "I"
              move "Confermi l'inserimento?" to wb-msg
            else
              move "Confermi la modifica?" to wb-msg
           end-if
           perform acc-conf-custom
           if not f3
              go to a-tm-tml
           end-if

      **---------------------------------------------------------**
      ** Inserimento: se sono in "Inserimento" devo anche modificare
      ** il padre dell'elemento "spostato"
      **---------------------------------------------------------**
           if a-operazione = "I" 
              if a-fl-ia = "I"
                 initialize fld-rec
                 move tm-pac          to fld-pac
                 move tm-prg          to fld-prg
                 move tm-frm          to fld-frm
                 move tm-pag          to fld-pag
                 move tm-prog (i-prec)   to fld-prog
                 perform rd-scrfield-lk
                 if w-verbo-invalido
                    string "L'elemento con progressivo " fld-prog "non "
                      "e` stato trovato!!" delimited size into wb-msg
                    perform vbx-msg-error
                  else
                    move tm-prog (i)  to fld-padre
                    perform rwr-scrfield
                 end-if
                 move i-prec          to a-i
               else
                 move i               to a-i
              end-if
              if a-i = 1
                 move 0               to tm-padre (i)
               else
                 compute tm-padre (i) = tm-prog (a-i - 1)
              end-if
           end-if

           move i                        to k
           perform x-riempi-grid-record
           move grid-record              to gdad-record

           initialize util-gdad
           if a-operazione = "I"
              move "Add-Record"          to gdad-ope
            else
              move "Modify-Record"       to gdad-ope
              move w-cell-y              to gdad-row-id
           end-if
           move prog-err                 to gdad-prg
           move 1                        to gdad-frm
           move k-id-grid                to gdad-ctrl-id
           call "GRIDADMN" using stringhe util-gdad gdad-record
           if a-operazione = "I"
              modify e-tm-tab,
                 num-rows = tm-pnt + 1,
                 cursor-y = i + 1,
                 cursor-x = 1
           end-if

      *******************************************************************
      * Eseguo l'aggiornamento                                          *
      *******************************************************************
           initialize fld-rec
           move tm-pac               to fld-pac
           move tm-prg               to fld-prg
           move tm-frm               to fld-frm
           move tm-pag               to fld-pag
           move tm-prog (i)          to fld-prog
           perform rd-scrfield-lk
           if w-verbo-ok
              if a-operazione = "I"
                 move "Attenzione: il controllo indicato e` gia` present
      -               "e sulla pagina in esame. Verra` variata con i dat
      -               "i attuali"    to wb-msg
                 perform vbx-msg
                 move "M"                to a-operazione
              end-if
            else
              if a-operazione = "M"
                 move "Attenzione: il controllo   indicato non e` piu` p
      -               "resente sulla pagina in esame. Verra` reinserito 
      -               "con i dati attuali"  to wb-msg
                 perform vbx-msg
                 move "I"                to a-operazione
              end-if
           end-if

           initialize fld-rec
           move tm-pac                   to fld-pac
           move tm-prg                   to fld-prg
           move tm-frm                   to fld-frm
           move tm-pag                   to fld-pag
           move tm-prog (i)              to fld-prog
           move tm-tml (i)               to fld-tml
           move tm-des (i)               to fld-des
           move tm-tip (i)               to fld-tip
           move tm-size (i)              to fld-size 
           move tm-size-dec (i)          to fld-size-dec
           move tm-h-size (i)            to fld-h-size
           evaluate fld-tip
            when k-ctr-alfanumerico
              move 1,1                   to fld-v-size
            when other
              move tm-v-size (i)         to fld-v-size
           end-evaluate
           move tm-v-pos (i)             to fld-v-pos
           move tm-h-pos (i)             to fld-h-pos
           move tm-id (i)                to fld-id
           move tm-id-assoluto (i)       to fld-id-assoluto
           move tm-nome (i)              to fld-nome
           move tm-fl-value (i)          to fld-fl-value
           move tm-indice (i)            to fld-indice
           move tm-enabled (i)           to fld-enabled
           move tm-visible (i)           to fld-visible
           move tm-align (i)             to fld-align
           move tm-label (i)             to fld-label
           move tm-label-h-pos-rel (i)   to fld-label-h-pos-rel
           move tm-label-v-pos-rel (i)   to fld-label-v-pos-rel
           move tm-label-h-size (i)      to fld-label-h-size
           move tm-label-v-size (i)      to fld-label-v-size
           move tm-color-control (i)     to fld-color-control
           move tm-color-label (i)       to fld-color-label
           move tm-liv (i)               to fld-liv
           move tm-padre (i)             to fld-padre
           move tm-fl-grid-frame (i)     to fld-fl-grid-frame
           move tm-fl-hnd-label (i)      to fld-fl-hnd-label
           move tm-fl-val-label (i)      to fld-fl-val-label
           move tm-lab-def (i)           to fld-lab-def
           move tm-fl-entry-point (i)    to fld-fl-entry-point
           move tm-fl-grid-dinamica (i)  to fld-fl-grid-dinamica
           move tm-fl-edit-grid (i)      to fld-fl-edit-grid
           move tm-fl-color-form (i)     to fld-fl-color-form
           move tm-case (i)              to fld-case 
           move tm-layout (i)            to fld-layout
           move tm-css-classe (i)        to fld-css-classe
           move tm-frame-style (i)       to fld-frame-style 
           move tm-fl-full-height (i)    to fld-fl-full-height
           move tm-fl-notify (i)         to fld-fl-notify
           move tm-fl-self-act (i)       to fld-fl-self-act
           move tm-fl-cent-head (i)      to fld-fl-cent-head
           move tm-grid-ctrl-a-ep (i)    to fld-grid-ctrl-a-ep
           move tm-grid-be-ep (i)        to fld-grid-be-ep
           move tm-disattiva-tf-ep (i)   to fld-disattiva-tf-ep
           move tm-fl-headings (i)       to fld-fl-headings
           move tm-grid-prf-col (i)      to fld-grid-prf-col
           move tm-grid-max-row (i)      to fld-grid-max-row
           move tm-check-true (i)        to fld-check-true
           move tm-check-false (i)       to fld-check-false
           move tm-s67-liv-ric (i)       to fld-s67-liv-ric
           move tm-u10-divisa  (i)       to fld-u10-divisa 
           move tm-u10-data (i)          to fld-u10-data
           move tm-u10-tipo-dato (i)     to fld-u10-tipo-dato 
           move tm-u10-edit-punti (i)    to fld-u10-edit-punti
           evaluate tm-s52-verifica (i)
            when "1"
              move "S"                   to fld-s52-verifica
            when "2"
              move "Z"                   to fld-s52-verifica
            when "3"
              move "N"                   to fld-s52-verifica
            when "4"
              move "A"                   to fld-s52-verifica
            when "5"
              move "a"                   to fld-s52-verifica
            when "6"
              move "M"                   to fld-s52-verifica
            when "7"
              move "m"                   to fld-s52-verifica
            when "8"
              move "G"                   to fld-s52-verifica
            when "9"
              move "g"                   to fld-s52-verifica
           end-evaluate
           evaluate tm-s93-verifica (i)
            when "1"
              move "S"                   to fld-s93-verifica
            when "2"
              move "Z"                   to fld-s93-verifica
            when "3"
              move "N"                   to fld-s93-verifica
           end-evaluate
           move tm-status-bar (i)        to fld-status-bar
           move tm-exception (i)         to fld-exception
           move tm-fl-sezione (i)        to fld-fl-sezione
           move tm-pb-bitmap (i)         to fld-pb-bitmap
           move tm-enabled-auto (i)      to fld-enabled-auto
           move tm-visible-auto (i)      to fld-visible-auto
           move tm-fl-evidenza (i)       to fld-fl-evidenza
           move tm-fl-secure (i)         to fld-fl-secure
           move tm-fl-pos-man (i)     to fld-fl-pos-man

           if a-operazione = "I"
              perform wr-scrfield
              if w-verbo-invalido
                 move "Errore in inserimento controllo!!" to wb-msg
                 perform vbx-msg
              end-if
              move tm-prog (i)           to tm-massimo
            else
              move tm-padre (i)          to fld-padre
              perform rwr-scrfield
              if w-verbo-invalido
                 move "Errore in modifica controllo!!" to wb-msg
                 perform vbx-msg
              end-if
           end-if

      **---------------------------------------------------------**
      ** Se la label ha un nome generico, la salvo li`
      **---------------------------------------------------------**
           if tm-lab-def (i) not = spaces and
              tm-label (i)   not = spaces
              initialize pgs-rec
              move spaces                to pgs-pac
              move spaces                to pgs-prg
              move k-scr-msg-label       to pgs-tip
              move tm-lab-def (i)        to pgs-id
              move 0                     to pgs-sez
              perform rd-scrpgmsg-lk
              if w-verbo-invalido
                 perform wr-scrpgmsg
              end-if
              move tm-label (i)          to pgs-msg
              perform rwr-scrpgmsg
           end-if

      **---------------------------------------------------------**
      ** Reimposto i valori x il prossimo campo
      ** L'id massimo viene impostato se non sono su un campo griglia
      **---------------------------------------------------------**
           if tm-v-pos (i) > tm-v-pos-max
              move tm-v-pos (i)    to tm-v-pos-max
              compute tm-h-pos-max = tm-h-pos (i) 
              compute tm-h-pos-fine-max = 
                 tm-h-pos (i) + tm-h-size (i) + 0,5
            else
              if tm-h-pos (i) > tm-h-pos-max
                 compute tm-h-pos-max = tm-h-pos (i) 
                 compute tm-h-pos-fine-max = 
                    tm-h-pos (i) + tm-h-size (i) + 0,5
              end-if
           end-if
           move tm-tip (i)      to a2-tip
           move tm-id (i)       to a2-id
           perform a2-imposta-id-max

      **---------------------------------------------------------**
      ** Se il controllo e` agganciato ad un template: 
      ** - Se il template non esiste lo creo ex novo
      ** - Se esiste posso modificare la descrizione
      **---------------------------------------------------------**
           if tm-tml (i) not = spaces
              initialize ftm-rec
              move tm-tml (i)      to ftm-cod
              perform rd-scrtempl
              if w-verbo-invalido
                 move tm-tip (i)   to ftm-tip
                 move tm-size (i)  to ftm-size
                 move tm-size-dec (i) to ftm-size-dec
                 move tm-h-size (i)   to ftm-h-size
                 move tm-v-size (i)   to ftm-v-size
                 move tm-label (i)    to ftm-label
                 move tm-label-h-pos-rel (i) to ftm-label-h-pos-rel
                 move tm-label-v-pos-rel (i) to ftm-label-v-pos-rel
                 move tm-label-h-size (i) to ftm-label-h-size
                 move tm-label-v-size (i) to ftm-label-v-size
                 move tm-fl-grid-frame (i) to ftm-fl-grid-frame
                 move tm-fl-hnd-label (i)  to ftm-fl-hnd-label
                 move tm-case (i)     to ftm-case
                 move tm-frame-style (i) to ftm-frame-style
                 move tm-fl-full-height (i) to ftm-fl-full-height
                 move tm-fl-notify (i)   to ftm-fl-notify
                 move tm-fl-cent-head (i)   to ftm-fl-cent-head
                 move tm-check-true (i)  to ftm-check-true
                 move tm-check-false (i) to ftm-check-false
                 move tm-lab-def (i)  to ftm-lab-def
                 perform wr-scrtempl
              end-if
              move tm-tml-des (i)     to ftm-des
              perform rwr-scrtempl
           end-if

      **-------------------------------------------------------**
      ** Inserimento condizioni di abilitazione e visibilita`
      **-------------------------------------------------------**
           if tm-enabled (i)          = "V"
              if tm-Cond-enabled (i) = spaces
                 initialize util-scrsrc
                 move "Cancella"            to scrsrc-ope
                 move tm-pac                to scrsrc-pac
                 move tm-prg                to scrsrc-prg
                 move "P"                   to scrsrc-div
                 move "CndEnabled"          to scrsrc-tip
                 move tm-nome (i)           to scrsrc-key
                 move tm-cond-enabled (i)   to scrsrc-source
                 call "SCRSRC"           using stringhe util-scrsrc
               else
                 initialize util-scrsrc
                 move "Scrivi"              to scrsrc-ope
                 move tm-pac                to scrsrc-pac
                 move tm-prg                to scrsrc-prg
                 move "P"                   to scrsrc-div
                 move "CndEnabled"          to scrsrc-tip
                 move tm-nome (i)           to scrsrc-key
                 move tm-cond-enabled (i)   to scrsrc-source
                 call "SCRSRC"           using stringhe util-scrsrc
              end-if
           end-if
           if tm-visible (i)          = "V"
              if tm-cond-visible (i) = spaces
                 initialize util-scrsrc
                 move "Cancella"            to scrsrc-ope
                 move tm-pac                to scrsrc-pac
                 move tm-prg                to scrsrc-prg
                 move "P"                   to scrsrc-div
                 move "CndVisible"          to scrsrc-tip
                 move tm-nome (i)           to scrsrc-key
                 move tm-cond-Visible (i)   to scrsrc-source
                 call "SCRSRC"           using stringhe util-scrsrc
               else
                 initialize util-scrsrc
                 move "Scrivi"              to scrsrc-ope
                 move tm-pac                to scrsrc-pac
                 move tm-prg                to scrsrc-prg
                 move "P"                   to scrsrc-div
                 move "CndVisible"          to scrsrc-tip
                 move tm-nome (i)           to scrsrc-key
                 move tm-cond-Visible (i)   to scrsrc-source
                 call "SCRSRC"           using stringhe util-scrsrc
              end-if
           end-if
           if (tm-tip (i) = k-ctr-grid or
               tm-tip (i) = k-ctr-grid-paged)
              if tm-cond-row-color (i) = spaces
                 initialize util-scrsrc
                 move "Cancella"            to scrsrc-ope
                 move tm-pac                to scrsrc-pac
                 move tm-prg                to scrsrc-prg
                 move "P"                   to scrsrc-div
                 move "RowColor"            to scrsrc-tip
                 move tm-nome (i)           to scrsrc-key
                 move tm-cond-row-color (i) to scrsrc-source
                 call "SCRSRC"           using stringhe util-scrsrc
               else
                 initialize util-scrsrc
                 move "Scrivi"              to scrsrc-ope
                 move tm-pac                to scrsrc-pac
                 move tm-prg                to scrsrc-prg
                 move "P"                   to scrsrc-div
                 move "RowColor"            to scrsrc-tip
                 move tm-nome (i)           to scrsrc-key
                 move tm-cond-row-color (i) to scrsrc-source
                 call "SCRSRC"           using stringhe util-scrsrc
              end-if
           end-if
           if tm-tip (i) = k-ctr-grid-paged
              if tm-gor-alt-key (i) = spaces
                 initialize util-scrsrc
                 move "Cancella"            to scrsrc-ope
                 move tm-pac                to scrsrc-pac
                 move tm-prg                to scrsrc-prg
                 move "E"                   to scrsrc-div
                 move "GorAltKey"           to scrsrc-tip
                 move tm-nome (i)           to scrsrc-key
                 move tm-gor-alt-key (i)    to scrsrc-source
                 call "SCRSRC"           using stringhe util-scrsrc
               else
                 initialize util-scrsrc
                 move "Scrivi"              to scrsrc-ope
                 move tm-pac                to scrsrc-pac
                 move tm-prg                to scrsrc-prg
                 move "E"                   to scrsrc-div
                 move "GorAltKey"           to scrsrc-tip
                 move tm-nome (i)           to scrsrc-key
                 move tm-gor-alt-key (i)    to scrsrc-source
                 call "SCRSRC"           using stringhe util-scrsrc
              end-if
           end-if

      **---------------------------------------------------------**
      ** L'utente ha variato la posizione del campo; chiedo se la 
      **  variazione deve essere apportata anche ai campi successivi
      **---------------------------------------------------------**
           if a-operazione = "M"
              if tm-v-pos-sav not = tm-v-pos (i) or
                 tm-h-pos-sav not = tm-h-pos (i)
                 string "Hai spostato la posizione del controllo; " 
                    k-newline
                   " applico la modifica anche ai campi successivi?" 
                    delimited size  into wb-msg
                 perform acc-conf-custom
                 if f3
                    compute a3-move-v = tm-v-pos (i) - tm-v-pos-sav
                    compute a3-move-h = tm-h-pos (i) - tm-h-pos-sav
                    move i         to a3-i
                    move tm-v-pos-sav      to a3-v-pos-sav
                    move tm-h-pos-sav      to a3-h-pos-sav
                    perform a3-sposta-controlli thru ex-a3
                 end-if
              end-if
           end-if

           move zero               to tm-sel-idx tm-sel-prog

           if a-operazione = "I" and
              a-fl-ia      = "I"
              go to a-inizio-dati
            else
      *       go to a-grid
              go to a-inizio-dati
           end-if
           .
      *
       fine.
       z-chiudi.
           close scrforms scrfmpag scrtempl scrfield scrfldvf scrpgmsg
                 scrfldex scrfiles scrpgfil
           close relpacch reldirpa relprog
           close window w-sv-cur-prg
           perform z-99-exit-program
           exit program.
      *
      **---------------------------------------------------------------*
      ** Conto i valori fissi presenti attualmente sull'informazione
      **---------------------------------------------------------------*
       a1-conta-valori.
           move zero               to a1-n-val
           initialize flv-rec
           move "C"                to flv-tip
           move tm-pagina          to flv-pagina
           move a1-prog            to flv-ctr-prog
           perform st-scrfldvf-notmin
           if w-verbo-invalido  go to a1-99.
       a1-10-loop.
           perform rdnxt-scrfldvf
           if w-fine-file       go to a1-99.

           if flv-tip      not = "C"       or
              flv-pagina   not = tm-pagina or
              flv-ctr-prog not = a1-prog
              go to a1-99
           end-if

           add 1                   to a1-n-val

           go to a1-10-loop.

       a1-99.
       ex-a1.  exit.

      **---------------------------------------------------------------*
      ** Imposto il valore dell'id massimo della pagina
      **---------------------------------------------------------------*
      ** Se ho inserito un controllo composto (piano conti, merce, ecc.)
      **   l'id massimo viene impostato in base al numero di campi
      **   previsti
      **---------------------------------------------------------**
       a2-imposta-id-max.
           if a2-id     > tm-id-max and
              a2-id not = 9999
              if a2-tip not = k-ctr-grid       and
                 a2-tip not = k-ctr-grid-paged
                 move a2-id        to tm-id-max
                 evaluate a2-tip
                  when k-ctr-piano-conti
                     add 3         to tm-id-max
                  when k-ctr-merce
                     add 7         to tm-id-max
                 end-evaluate
              end-if
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Spostamento controlli; 
      ** - Spostamento verticale: sposto tutti i controlli della riga
      **   e delle righe successive
      ** - Spostamento orizzontale: sposto tutti i controlli della riga
      ** Lo spostamento avviene, per la riga corrente, solo dal controllo
      **   su cui si e` posizionati
      **---------------------------------------------------------------*
       a3-sposta-controlli.

           perform with test before until a3-i = tm-pnt
              add 1                to a3-i
              if tm-v-pos (a3-i) = a3-v-pos-sav
                 add a3-move-h     to tm-h-pos (a3-i)
                 add a3-move-v     to tm-v-pos (a3-i)
               else
                 if tm-v-pos (a3-i) > a3-v-pos-sav
                    add a3-move-v  to tm-v-pos (a3-i)
                 end-if
              end-if
              move tm-pac            to fld-pac   
              move tm-prg            to fld-prg   
              move tm-frm            to fld-frm   
              move tm-pag            to fld-pag   
              move tm-prog (a3-i)    to fld-prog
              perform rd-scrfield-lk
              if w-verbo-invalido
                 string "Non trovato controllo: " fld-prog
                    delimited size into wb-msg
                 perform vbx-msg-error
                 exit perform cycle
              end-if
              move tm-v-pos (a3-i)   to fld-v-pos
              move tm-h-pos (a3-i)   to fld-h-pos
              perform rwr-scrfield
           end-perform

      **---------------------------------------------------------------*
      ** Aggiornamento SCRFORMS per avere la data aggiornamento 
      **---------------------------------------------------------------*
           move tm-pac               to fld-pac   
           move tm-prg               to fld-prg   
           move tm-frm               to fld-frm   
           perform rd-scrforms-lk
           if w-verbo-ok
              perform rwr-scrforms
           end-if
           

           .
           

       a3-999.
       ex-a3.  exit.
      *
      **---------------------------------------------------------------*
      ** La riga selezionata deve essere spostata in su nella sequenza
      ** 1: la riga successiva, se presente, diventa "orfana"
      ** 2: la riga precedente assume come padre la riga attuale
      ** 3: la riga selezionata assume il padre di quella precedente
      ** 4: la riga successiva, se presente, assume come padre quella prec.
      **---------------------------------------------------------------*
       a3-sposta-su.
           if i not = tm-pnt
              move tm-pac            to fld-pac   
              move tm-prg            to fld-prg   
              move tm-frm            to fld-frm   
              move tm-pag            to fld-pag   
              move tm-prog (i + 1)   to fld-prog
              perform rd-scrfield-lk
              move 9999              to fld-padre
              perform rwr-scrfield
           end-if

           move tm-pac               to fld-pac   
           move tm-prg               to fld-prg   
           move tm-frm               to fld-frm   
           move tm-pag               to fld-pag   
           move tm-prog (i - 1)      to fld-prog
           perform rd-scrfield-lk
           move fld-padre            to a3-padre-riga-prec
           move tm-prog (i)          to fld-padre
           perform rwr-scrfield

           move tm-pac               to fld-pac   
           move tm-prg               to fld-prg   
           move tm-frm               to fld-frm   
           move tm-pag               to fld-pag   
           move tm-prog (i)          to fld-prog
           perform rd-scrfield-lk
           move a3-padre-riga-prec   to fld-padre
           perform rwr-scrfield

           if i not = tm-pnt
              move tm-pac            to fld-pac   
              move tm-prg            to fld-prg   
              move tm-frm            to fld-frm   
              move tm-pag            to fld-pag   
              move tm-prog (i + 1)   to fld-prog
              perform rd-scrfield-lk
              move tm-prog (i - 1)   to fld-padre
              perform rwr-scrfield
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** La riga selezionata deve essere spostata in giu nella sequenza
      ** 1: la riga + 2, se presente, diventa "orfana"
      ** 2: la riga selezionata assume il padre della riga + 2
      ** 4: la riga successiva, assume come padre quello della riga attuale
      ** 2: la riga + 2, se presente, assume come padre la riga attuale
      **---------------------------------------------------------------*
       a3-sposta-giu.
           if i <= (tm-pnt + 2) 
              move tm-pac            to fld-pac   
              move tm-prg            to fld-prg   
              move tm-frm            to fld-frm   
              move tm-pag            to fld-pag   
              move tm-prog (i + 2)   to fld-prog
              perform rd-scrfield-lk
              move fld-padre         to a3-padre-riga-dopo
              move 9999              to fld-padre
              perform rwr-scrfield
           end-if

           move tm-pac               to fld-pac   
           move tm-prg               to fld-prg   
           move tm-frm               to fld-frm   
           move tm-pag               to fld-pag   
           move tm-prog (i)          to fld-prog
           perform rd-scrfield-lk
           move fld-padre            to a3-padre-riga-att
           move a3-padre-riga-dopo   to fld-padre
           perform rwr-scrfield

           move tm-pac               to fld-pac   
           move tm-prg               to fld-prg   
           move tm-frm               to fld-frm   
           move tm-pag               to fld-pag   
           move tm-prog (i + 1)      to fld-prog
           perform rd-scrfield-lk
           move a3-padre-riga-att    to fld-padre
           perform rwr-scrfield

           if i <= (tm-pnt + 2) 
              move tm-pac            to fld-pac   
              move tm-prg            to fld-prg   
              move tm-frm            to fld-frm   
              move tm-pag            to fld-pag   
              move tm-prog (i + 2)   to fld-prog
              perform rd-scrfield-lk
              move tm-prog (i)       to fld-padre
              perform rwr-scrfield
           end-if
           .
      *
      **---------------------------------------------------------------*
      ** Copio i valori fissi dalla riga selezionata
      **---------------------------------------------------------------*
       a4-copia-vf.
           initialize flv-rec
           move "C"                to flv-tip
           move tm-pagina          to flv-pagina
           move a4-prog-da         to flv-ctr-prog
           perform st-scrfldvf-notmin
           if w-verbo-invalido  go to a4-999.
       a4-10-loop.
           perform rdnxt-scrfldvf
           if w-fine-file       go to a4-999.

           if flv-tip      not = "C"       or
              flv-pagina   not = tm-pagina or
              flv-ctr-prog not = a4-prog-da
              go to a4-999
           end-if

           move flv-chia1          to a4-flv-chia1

           move a4-prog-a          to flv-ctr-prog
           perform wr-scrfldvf

           move a4-flv-chia1       to flv-chia1
           perform st-scrfldvf-equal
           perform rdnxt-scrfldvf
           go to a4-10-loop.


       a4-999.
       ex-a4.  exit.

      *
       d-carica-dati.
           initialize tm-tab
           move zero                     to tm-pnt tm-id-max
           move zero                     to i
           move zero                     to tm-massimo
           move zero                     to tm-v-pos-max tm-h-pos-max
                                            tm-h-pos-fine-max
      **---------------------------------------------------------------**
      * Carico gli elementi gia' esistenti attraverso il prog.padre;
      * Parto con quello che ha padre = 0
      **---------------------------------------------------------------**
           move 0                        to c-padre
           move "00"                     to d-liv-prec
           .

       d-carica-dati-loop.
           initialize fld-rec
           move tm-pac                   to fld-pac
           move tm-prg                   to fld-prg
           move tm-frm                   to fld-frm
           move tm-pag                   to fld-pag
           move c-padre                  to fld-padre
           perform rd-scrfield-chia4
           if w-verbo-invalido  
              go to d-carica-dati-fine
           end-if

      *------------------------------------------------------------
      * controllo il livello: se aumenta, il campo precedente deve
      * essere un gruppo
      *------------------------------------------------------------
           if d-liv-prec = "00" 
              move fld-liv         to d-liv-prec
           end-if

           if fld-liv > d-liv-prec
              if d-fld-tip-prec <> k-ctr-gruppo
                 string "attenzione, trovato livello errato prima di '" 
                    fld-des "'"       delimited "  " into wb-msg
                 perform vbx-msg-warning
              end-if
           end-if
           move fld-liv            to d-liv-prec
           move fld-tip            to d-fld-tip-prec

           if i = k-max-ele-tab
              move "Tabella Piena! Ulteriori elementi scartati." to wb-1
              move "Contattare il personale Eurosystem2000" to wb-2
              perform box-msg
              go to d-carica-dati-fine
           end-if
           add 1                         to i
           initialize tm-righe (i)
           move fld-prog                 to tm-prog (i)
           move fld-tml                  to tm-tml (i)
           move fld-des                  to tm-des (i)
           move fld-tip                  to tm-tip (i)
           move fld-size                 to tm-size (i)
           move fld-size-dec             to tm-size-dec (i)
           move fld-h-size               to tm-h-size (i)
           move fld-v-size               to tm-v-size (i)
           move fld-h-pos                to tm-h-pos (i)
           move fld-v-pos                to tm-v-pos (i)
           move fld-id                   to tm-id (i)
           move fld-id-assoluto          to tm-id-assoluto (i)
           move fld-nome                 to tm-nome (i)
           move fld-fl-value             to tm-fl-value (i)
           move fld-u10-edit-punti       to tm-u10-edit-punti (i)
           move fld-indice               to tm-indice (i)
           move fld-enabled              to tm-enabled (i)
           move fld-visible              to tm-visible (i)
           move fld-align                to tm-align (i)
           move fld-label                to tm-label (i)
           move fld-label-h-pos-rel      to tm-label-h-pos-rel (i)
           move fld-label-v-pos-rel      to tm-label-v-pos-rel (i)
           move fld-label-h-size         to tm-label-h-size (i)
           move fld-label-v-size         to tm-label-v-size (i)
           move fld-liv                  to tm-liv (i)
           move fld-padre                to tm-padre (i)
           move fld-fl-grid-frame        to tm-fl-grid-frame (i)
           move fld-fl-hnd-label         to tm-fl-hnd-label (i)
           move fld-fl-val-label         to tm-fl-val-label (i)
           move fld-lab-def              to tm-lab-def (i)
           move fld-fl-entry-point       to tm-fl-entry-point (i)
           move fld-fl-grid-dinamica     to tm-fl-grid-dinamica (i)
           if tm-fl-grid-dinamica (i) <> "N" and
              tm-fl-grid-dinamica (i) <> "S"
              if tm-tip (i) = k-ctr-grid       or   
                 tm-tip (i) = k-ctr-grid-paged     
                 move "S"                to tm-fl-grid-dinamica (i)
              end-if
           end-if
           move fld-fl-edit-grid         to tm-fl-edit-grid (i)
           if tm-fl-edit-grid (i) <> "N" and
              tm-fl-edit-grid (i) <> "A" and
              tm-fl-edit-grid (i) <> "S"
              move "N"                   to tm-fl-edit-grid (i)
           end-if
           move fld-fl-color-form        to tm-fl-color-form (i)
           move fld-case                 to tm-case (i)
           move fld-color-control        to tm-color-control (i)
           move fld-color-label          to tm-color-label (i)
           move fld-layout               to tm-layout (i)
           move fld-css-classe           to tm-css-classe (i)
           move fld-frame-style          to tm-frame-style (i)
           move fld-fl-full-height       to tm-fl-full-height (i)
           move fld-fl-notify            to tm-fl-notify (i)
           move fld-fl-self-act          to tm-fl-self-act (i)
           move fld-fl-cent-head         to tm-fl-cent-head (i)
           move fld-grid-ctrl-a-ep       to tm-grid-ctrl-a-ep (i)
           move fld-grid-be-ep           to tm-grid-be-ep (i)
           move fld-disattiva-tf-ep      to tm-disattiva-tf-ep (i)
           move fld-fl-headings          to tm-fl-headings (i)
           if tm-fl-headings (i) <> "S" and
              tm-fl-headings (i) <> "N" 
              if tm-tip (i) = k-ctr-grid       or
                 tm-tip (i) = k-ctr-grid-paged
                 move "S"                to tm-fl-headings (i)
               else
                 move "N"                to tm-fl-headings (i)
              end-if
           end-if
           move fld-grid-prf-col         to tm-grid-prf-col(i)
           if fld-grid-max-row not numeric
              move 0                     to fld-grid-max-row
           end-if
           move fld-grid-max-row         to tm-grid-max-row(i)
           move fld-check-true           to tm-check-true (i)
           move fld-check-false          to tm-check-false (i)
           move fld-s67-liv-ric          to tm-s67-liv-ric (i)
           move fld-u10-divisa           to tm-u10-divisa  (i)
           move fld-u10-data             to tm-u10-data (i)
           move fld-u10-tipo-dato        to tm-u10-tipo-dato (i)
           evaluate fld-s52-verifica
            when "S"
              move "1"                   to tm-s52-verifica (i)
            when "Z"
              move "2"                   to tm-s52-verifica (i)
            when "N"
              move "3"                   to tm-s52-verifica (i)
            when "A"
              move "4"                   to tm-s52-verifica (i)
            when "a"
              move "5"                   to tm-s52-verifica (i)
            when "M"
              move "6"                   to tm-s52-verifica (i)
            when "m"
              move "7"                   to tm-s52-verifica (i)
            when "G"
              move "8"                   to tm-s52-verifica (i)
            when "g"
              move "9"                   to tm-s52-verifica (i)
           end-evaluate
           evaluate fld-s93-verifica
            when "S"
              move "1"                   to tm-s93-verifica (i)
            when "Z"
              move "2"                   to tm-s93-verifica (i)
            when "N"
              move "3"                   to tm-s93-verifica (i)
           end-evaluate
           move fld-status-bar           to tm-status-bar (i)
           move fld-exception            to tm-exception (i)
           move fld-fl-sezione           to tm-fl-sezione (i)
           move fld-pb-bitmap            to tm-pb-bitmap (i)
           move fld-enabled-auto         to tm-enabled-auto (i)
           if tm-enabled-auto (i) not = "S" and
              tm-enabled-auto (i) not = "N" 
              move "N"                   to tm-enabled-auto (i)
           end-if
           move fld-visible-auto         to tm-visible-auto (i)
           if tm-visible-auto (i) not = "S" and
              tm-visible-auto (i) not = "N" 
              move "N"                   to tm-visible-auto (i)
           end-if
           move fld-fl-evidenza          to tm-fl-evidenza (i)
           move fld-fl-secure            to tm-fl-secure (i)
           move fld-fl-pos-man        to tm-fl-pos-man (i)

           if fld-tml not = spaces
              initialize ftm-rec
              move fld-tml               to ftm-cod
              perform rd-scrtempl
              move ftm-des               to tm-tml-des (i)
           end-if

           if fld-prog > tm-massimo
              move fld-prog        to tm-massimo
           end-if
           move fld-tip            to a2-tip
           move fld-id             to a2-id
           perform a2-imposta-id-max
           if fld-id     > tm-id-max and
              fld-id not = 9999
              move fld-id          to tm-id-max
           end-if
      *-------------------------------------------------------------
      * Caricamento HELP CONTEXT ID
      *-------------------------------------------------------------
           initialize fldx-rec
           move fld-chia1               to fldx-chia1
           perform rd-scrfldex
           if w-verbo-invalido
              move "N"                  to fldx-help-context-attivo
           end-if
           move fldx-id                 to tm-hc-id (i)
           move fldx-help-context-attivo   to tm-hc-attivo (i)

           move fldx-id-controllo       to tm-fldx-id-controllo (i)

      **-------------------------------------------------------**
      ** Caricamento condizione enabled
      **-------------------------------------------------------**
           if tm-enabled (i) = "V"
              initialize util-scrsrc
              move "Leggi"                  to scrsrc-ope
              move tm-pac                   to scrsrc-pac
              move tm-prg                   to scrsrc-prg
              move "P"                      to scrsrc-div
              move "CndEnabled"             to scrsrc-tip
              move tm-nome (i)              to scrsrc-key
              call "SCRSRC"              using stringhe util-scrsrc
              move scrsrc-source            to tm-cond-enabled (i)
           end-if

      **-------------------------------------------------------**
      ** Caricamento condizione visible
      **-------------------------------------------------------**
           if tm-visible (i) = "V"
              initialize util-scrsrc
              move "Leggi"                  to scrsrc-ope
              move tm-pac                   to scrsrc-pac
              move tm-prg                   to scrsrc-prg
              move "P"                      to scrsrc-div
              move "CndVisible"             to scrsrc-tip
              move tm-nome (i)              to scrsrc-key
              call "SCRSRC"              using stringhe util-scrsrc
              move scrsrc-source            to tm-cond-visible (i)
           end-if

      **-------------------------------------------------------**
      ** Caricamento condizione cambio colore riga griglia
      **-------------------------------------------------------**
           if tm-tip (i) = k-ctr-grid       or
              tm-tip (i) = k-ctr-grid-paged
              initialize util-scrsrc
              move "Leggi"                  to scrsrc-ope
              move tm-pac                   to scrsrc-pac
              move tm-prg                   to scrsrc-prg
              move "P"                      to scrsrc-div
              move "RowColor"               to scrsrc-tip
              move tm-nome (i)              to scrsrc-key
              call "SCRSRC"              using stringhe util-scrsrc
              move scrsrc-source            to tm-cond-row-color (i)
           end-if

      **-------------------------------------------------------**
      ** Caricamento chiavi alternative transito
      **-------------------------------------------------------**
           if tm-tip (i) = k-ctr-grid-paged   
              initialize util-scrsrc
              move "Leggi"                  to scrsrc-ope
              move tm-pac                   to scrsrc-pac
              move tm-prg                   to scrsrc-prg
              move "E"                      to scrsrc-div
              move "GorAltKey"              to scrsrc-tip
              move tm-nome (i)              to scrsrc-key
              call "SCRSRC"              using stringhe util-scrsrc
              move scrsrc-source            to tm-gor-alt-key (i)
           end-if


      **---------------------------------------------------------------**
      * Determino qual'e` la posizione dell'ultimo campo della pagina
      **---------------------------------------------------------------**
           if tm-v-pos (i) > tm-v-pos-max
              move tm-v-pos (i)    to tm-v-pos-max
              compute tm-h-pos-max = tm-h-pos (i) 
              compute tm-h-pos-fine-max = 
                 tm-h-pos (i) + tm-h-size (i) + 0,5
            else
              if tm-h-pos (i) > tm-h-pos-max
                 compute tm-h-pos-max = tm-h-pos (i) 
                 compute tm-h-pos-fine-max = 
                    tm-h-pos (i) + tm-h-size (i) + 0,5
              end-if
           end-if

           move fld-prog           to c-padre           
           go to d-carica-dati-loop
           .
       d-carica-dati-fine.
           if tm-id-max = 0
              compute tm-id-max = ((tm-pag - 1) * 100)
           end-if
           .
       ex-d. exit.

       e-scan.
           initialize w-clipboard
           move zero               to buf-size
           move 0                  to e-pagine
           move 0                  to e-errori

      *---------------------------------------------------------------
      * Scansione form
      *---------------------------------------------------------------
           initialize scr-rec
           perform st-scrforms-notmin
           if w-verbo-ok
              perform with test before until w-fine-file
                 perform rdnxt-scrforms
                 if w-verbo-ok 
                    move scr-chia1      to e1-form
                    perform e1-scansione-form
                  else
                    move k-fine-file   to statusfi
                 end-if
              end-perform
           end-if
           move k-verbo-ok             to statusfi

           if buf-size > 0
              display entry-field        
                handle in handle-ef,
                visible = 0
              modify handle-ef, value = w-clipboard
              modify handle-ef, cursor = -1
              modify handle-ef, action = action-copy
              destroy handle-ef
              
              move e-pagine             to zeta8a
              move e-errori             to zeta8b
              string "Problemi negli appunti (" zeta8a " pagine, "
                 zeta8b " errori)" 
                 delimited size       into wb-msg
              perform vbx-msg-info
           end-if
           .

       e1-scansione-form.
           move 0                       to e1-pag
           perform e1-scan-pagina thru ex-e1
           add 1                        to e-pagine

           initialize fpg-rec
           move e1-form                 to fpg-form
           perform st-scrfmpag-notmin
           if w-verbo-ok
              perform with test before until w-fine-file
                 perform rdnxt-scrfmpag
                 if w-verbo-ok         and
                    fpg-form = e1-form
                    move fpg-pag        to e1-pag
                    perform e1-scan-pagina thru ex-e1
                    add 1               to e-pagine
                  else
                    move k-fine-file   to statusfi
                 end-if
              end-perform
           end-if
           move k-verbo-ok             to statusfi
           .
           
      **---------------------------------------------------------------**
      ** Carico i controlli di una pagina
      **---------------------------------------------------------------**
       e1-scan-pagina.
           move "00"                    to e1-liv-prec
           
      **---------------------------------------------------------------**
      * Carico gli elementi gia' esistenti attraverso il prog.padre;
      * Parto con quello che ha padre = 0
      **---------------------------------------------------------------**
           move 0                        to e1-padre
           .
            
       e1-10-loop.
           initialize fld-rec
           move e1-form            to fld-form
           move e1-pag             to fld-pag
           move e1-padre           to fld-padre
           perform rd-scrfield-chia4
           if w-verbo-invalido
              go to e1-999
           end-if

           move fld-prog           to e1-padre           

      *------------------------------------------------------------
      * Controllo il livello: se aumenta, il campo precedente deve
      * essere un gruppo
      *------------------------------------------------------------
           if e1-liv-prec = "00" 
              move fld-liv         to e1-liv-prec
           end-if

           if fld-liv > e1-liv-prec
              if e1-fld-tip-prec <> k-ctr-gruppo
                 if (buf-size +  100) > 64000
                    move "Il buffer di scambio con gli appunti e` pieno;
      -               "l'esportazione e` parziale!!" to wb-msg
                    perform vbx-msg-warning
                 end-if

                 move e1-pac       to w-clipboard (buf-size + 1:8)
                 add 8             to buf-size
                 move x'09'        to w-clipboard (buf-size + 1:1)
                 add 1             to buf-size

                 move e1-prg       to w-clipboard (buf-size + 1:8)
                 add 8             to buf-size
                 move x'09'        to w-clipboard (buf-size + 1:1)
                 add 1             to buf-size

                 move e1-frm       to w-clipboard (buf-size + 1:2)
                 add 2             to buf-size
                 move x'09'        to w-clipboard (buf-size + 1:1)
                 add 1             to buf-size

                 move e1-pag       to w-clipboard (buf-size + 1:2)
                 add 2             to buf-size
                 move x'0D'        to w-clipboard (buf-size + 1:1)
                 move x'0A'        to w-clipboard (buf-size + 2:1)
                 add 2             to buf-size

                 add 1             to e-errori
              end-if
           end-if
           move fld-liv            to e1-liv-prec
           move fld-tip            to e1-fld-tip-prec

           go to e1-10-loop.

       e1-999.
       ex-e1.  exit.

      *-----------------------------------------------------------------
      * Rigenero i dati del programma corrente di gestione tabella
      * 1) Pulisco i dati
      * 2) Copio i dati dal template F-TEMPL
      *-----------------------------------------------------------------
       f-template-gestione-tabella.
           if tm-versione <> k-versione-ges-tabella
              move k-versione-ges-tabella  to zeta3
              if tm-versione = 0
                 move "Nuovo programma di gestione tabella. Genero la fo
      -           "rm."                 to wb-msg
               else
                 string "Gestione tabella di versione diversa: " 
                    tm-versione "; " k-newline
                   "Versione attuale: " zeta3 "; lo rigenero." 
                    delimited size    into wb-msg
              end-if
              perform vbx-msg-info
            else
              exit paragraph
           end-if

           initialize util-scrdup
           move "SCRDUP-CALLED" to scrdup-called
           move prog-err        to scrdup-caller
           move tm-pac          to scrdup-pac
           move "F-TEMPL"       to scrdup-prg
           move 1               to scrdup-frm
           move 0               to scrdup-pag
           move tm-prg          to scrdup-prg-dest
           call "SCRDUP"     using stringhe util-scrdup
           cancel "SCRDUP"
      * Inserisco il file da usare
      
           initialize gfil-rec
           move tm-nome-tabella         to gfil-nome
           perform rd-scrfiles
           if w-verbo-ok
              move tm-pac               to pgf-pac
              move tm-prg               to pgf-prg
              move gfil-tip              to pgf-tip
              move gfil-tab              to pgf-tab
              move gfil-num              to pgf-num
              move gfil-nome             to pgf-nom
              move "W"                  to pgf-open
              move "N"                  to pgf-fl-ext
              perform wr-scrpgfil
           end-if
      * Aggiungo altri files generici usati
      * SCRFDIPT
           initialize gfil-rec
           move "scrfdipt"              to gfil-nome
           perform rd-scrfiles
           if w-verbo-ok
              move tm-pac               to pgf-pac
              move tm-prg               to pgf-prg
              move gfil-tip              to pgf-tip
              move gfil-tab              to pgf-tab
              move gfil-num              to pgf-num
              move gfil-nome             to pgf-nom
              move "W"                  to pgf-open
              move "N"                  to pgf-fl-ext
              perform wr-scrpgfil
           end-if
      * SCRFDIPD
           initialize gfil-rec
           move "scrfdipd"              to gfil-nome
           perform rd-scrfiles
           if w-verbo-ok
              move tm-pac               to pgf-pac
              move tm-prg               to pgf-prg
              move gfil-tip              to pgf-tip
              move gfil-tab              to pgf-tab
              move gfil-num              to pgf-num
              move gfil-nome             to pgf-nom
              move "W"                  to pgf-open
              move "N"                  to pgf-fl-ext
              perform wr-scrpgfil
           end-if
      * SCRFILES
           initialize gfil-rec
           move "scrfiles"              to gfil-nome
           perform rd-scrfiles
           if w-verbo-ok
              move tm-pac               to pgf-pac
              move tm-prg               to pgf-prg
              move gfil-tip              to pgf-tip
              move gfil-tab              to pgf-tab
              move gfil-num              to pgf-num
              move gfil-nome             to pgf-nom
              move "W"                  to pgf-open
              move "N"                  to pgf-fl-ext
              perform wr-scrpgfil
           end-if
      * SCRFPATH
           initialize gfil-rec
           move "scrfpath"              to gfil-nome
           perform rd-scrfiles
           if w-verbo-ok
              move tm-pac               to pgf-pac
              move tm-prg               to pgf-prg
              move gfil-tip              to pgf-tip
              move gfil-tab              to pgf-tab
              move gfil-num              to pgf-num
              move gfil-nome             to pgf-nom
              move "W"                  to pgf-open
              move "N"                  to pgf-fl-ext
              perform wr-scrpgfil
           end-if
           .
      

      **---------------------------------------------------------------**
      * Cancellazione di un record                                      *
      * Cancello il record stesso, poi leggo l'elemento che lo aveva come
      *  padre e gli metto il padre dell'elemento cancellato
      **---------------------------------------------------------------**
       c-cancella.
           initialize fld-rec
           move tm-pac                   to fld-pac
           move tm-prg                   to fld-prg
           move tm-frm                   to fld-frm
           move tm-pag                   to fld-pag
           move tm-prog (i)              to fld-prog
           perform rd-scrfield-lk
           if w-verbo-invalido
              move "Il controllo selezionata non e` piu` presente sulla 
      -          "pagina!"               to wb-msg
              perform vbx-msg
              go to c-99
           end-if
           move fld-padre                to c-padre
           perform del-scrfield

           initialize fld-rec
           move tm-pac                   to fld-pac
           move tm-prg                   to fld-prg
           move tm-frm                   to fld-frm
           move tm-pag                   to fld-pag
           move tm-prog (i)              to fld-padre
           perform rd-scrfield-chia4-lk
           if w-verbo-ok
              move c-padre               to fld-padre
              perform rwr-scrfield
           end-if

      **---------------------------------------------------------------**
      ** Cancello i valori fissi
      **---------------------------------------------------------------**
           initialize flv-rec
           move "C"                to flv-tip
           move tm-pagina          to flv-pagina
           move tm-prog (i)        to flv-ctr-prog
           perform st-scrfldvf-notmin
           if w-verbo-invalido  go to  c-30.
        c-10-loop.
           perform rdnxt-scrfldvf
           if w-fine-file       go to  c-30.

           if flv-tip      not = "C"         or
              flv-pagina   not = tm-pagina   or
              flv-ctr-prog not = tm-prog (i)
              go to c-30
           end-if

           perform del-scrfldvf

           go to c-10-loop.

      **---------------------------------------------------------------**
      ** Cancello i pezzi di sorgente collegati
      ** 16-1-07: Per il momento disabilito la cancellazione, perche` in 
      **          caso di duplicazione pagina, i campi che cancello nella
      **          nuova pagina hanno lo stesso nome e segano anche queste
      **          informazioni!!
      **          Per migliorare la cosa: in SCRSRC, cancellare solo se 
      **            il campo non e` piu` presente nel programma
      **---------------------------------------------------------------**
       c-30.
      *    initialize util-scrsrc
      *    move "Cancella"               to scrsrc-ope
      *    move tm-pac                   to scrsrc-pac
      *    move tm-prg                   to scrsrc-prg
      *    move "P"                      to scrsrc-div
      *    move "CndEnabled"             to scrsrc-tip
      *    move tm-nome (i)              to scrsrc-key
      *    call "SCRSRC"              using stringhe util-scrsrc

      *    initialize util-scrsrc
      *    move "Cancella"               to scrsrc-ope
      *    move tm-pac                   to scrsrc-pac
      *    move tm-prg                   to scrsrc-prg
      *    move "P"                      to scrsrc-div
      *    move "CndVisible"             to scrsrc-tip
      *    move tm-nome (i)              to scrsrc-key
      *    call "SCRSRC"              using stringhe util-scrsrc

           .
       c-99.
       ex-c. exit.
      *
      **-----------------------------------------------------------**
      ** Controllo se posso eseguire la cancellazione
      ** Non posso se:
      **-----------------------------------------------------------**
       c1-ctrl-canc.
           move 'S'                           to c-ok
           .

       c1-99.
       ex-c1.  exit.

      *---------------------------------------------------------------**
      * Chiamata a SCREXLNG
      *---------------------------------------------------------------**
       x-call-screxlng.
           call "SCREXLNG"      using stringhe
                                   on overflow
              move "'SCREXLNG' non trovato." to wb-msg
              perform vbx-msg-error
           end-call
           cancel "SCREXLNG"
           .
      *---------------------------------------------------------------**
      * Chiamata a SCRIMLNG
      *---------------------------------------------------------------**
       x-call-scrimlng.
           call "SCRIMLNG"      using stringhe
                                   on overflow
              move "'SCRIMLNG' non trovato." to wb-msg
              perform vbx-msg-error
           end-call
           cancel "SCRIMLNG"
           .
      *---------------------------------------------------------------**
      * Chiamata a GESLNG
      *---------------------------------------------------------------**
       x-call-coge0a.
           initialize util-coge0a
           call "COGE0A"                using stringhe util-coge0a
           cancel "COGE0A"
           .

      *---------------------------------------------------------------**
      * Chiamate a GESLNG
      *---------------------------------------------------------------**
       x-call-scrlng-testi-programma.
           if tm-pac = " " or
              tm-prg = " "
              move "Seleziona prima pacchetto/programma!!" to wb-msg
              perform vbx-msg-error
              exit paragraph
           end-if

           initialize util-scrlng
           move "P"                     to scrlng-tipo
           move tm-pac                  to scrlng-pac
           move tm-prg                  to scrlng-prg
           call "SCRLNG"             using stringhe util-scrlng
           cancel "SCRLNG"
           .
       x-call-scrlng-testi-generali.
           initialize util-scrlng
           move "G"                     to scrlng-tipo
           call "SCRLNG"             using stringhe util-scrlng
           cancel "SCRLNG"
           .
      *---------------------------------------------------------------**
      * Chiamate a SCRPGNAV
      *---------------------------------------------------------------**
       x-call-navigazione.
           if tm-pac = " " or
              tm-prg = " "
              move "Seleziona prima pacchetto/programma!!" to wb-msg
              perform vbx-msg-error
              exit paragraph
           end-if

           initialize util-scrpgnav
           move "SCRPGNAV-CALLED"       to scrpgnav-called
           move "P"                     to scrpgnav-rs-tip
           move tm-prg                  to scrpgnav-rs-cod
           call "SCRPGNAV"           using stringhe util-scrpgnav
           cancel "SCRPGNAV"

           .
      *---------------------------------------------------------------**
      * Chiamate a SCRERS
      *---------------------------------------------------------------**
       x-call-generazione-nav.
           if tm-pac = " " or
              x-prg = " "
              move "Seleziona prima pacchetto/programma!!" to wb-msg
              perform vbx-msg-error
              exit paragraph
           end-if

           initialize util-scrers
           move "SCRSIM"             to scrers-caller
           move "SCRERS-CALLED"      to scrers-called
           move tm-pac               to scrers-pac
           move function lower-case(x-prg) to scrers-prg
           move x-silent-mode        to scrers-silent-mode
           call "SCRERS"          using stringhe util-scrers
           cancel "SCRERS"
           .
      *---------------------------------------------------------------**
      * Chiamate a SCRGFIL
      *---------------------------------------------------------------**
       x-call-scrgfil.
           initialize util-scrgfil
           call "SCRGFIL"         using stringhe util-scrgfil
           cancel "SCRGFIL"
           .

      *---------------------------------------------------------------**
      * Chiamate a SCRGTMPL
      *---------------------------------------------------------------**
       x-call-scrgtmpl.
           call "SCRGTMPL"         using stringhe util-scrgtmpl
           cancel "SCRGTMPL"
           .

      *---------------------------------------------------------------**
      * Chiamate a SCRGFDIP
      *---------------------------------------------------------------**
       x-call-scrgfdip.
           call "SCRGFDIP"         using stringhe util-scrgfdip
           cancel "SCRGFDIP"
           .
      *---------------------------------------------------------------**
      * Chiamate a SCRREL
      *---------------------------------------------------------------**
       x-call-scrrel-rilascio-spot.
           if tm-pac = " " or
              tm-prg = " "
              move "Seleziona prima pacchetto/programma!!" to wb-msg
              perform vbx-msg-error
              exit paragraph
           end-if

           move "Vuoi inizializzare i file EXP?" to wb-msg
           perform acc-conf-custom
           move "S"                     to fl-aggiungi-files
           if f3
              move "N"                  to fl-aggiungi-files
           end-if

           move "Inizio esportazione..." to wb-msg
           perform clock-msg
           perform view-clk
           perform call-scrrel-inizio
           move "Esportazione programma in corso..." to wb-msg
           perform view-clk
           perform call-scrrel-copia-agg-prog
           move "Esportazione dati generali in corso..." to wb-msg
           perform view-clk
           perform call-scrrel-fine
           perform rem-clk
           initialize wb-msg
           string "Esportazione terminata nei files"
                    " exp*.arc ed exp*.seq"
                k-newline "presenti in c:\ecoge\tmp"
                delimited size into wb-msg
           perform vbx-msg
           
           .

       call-scrrel-inizio.
           initialize util-scrrel
           move "RILASCIA-INIZIO"       to scrrel-ope
           move ext-prg-dir             to scrrel-path-in
           move "c:\ecoge\tmp"          to scrrel-path-out
           move fl-aggiungi-files       to scrrel-fl-aggiungi
           call "SCRREL"             using stringhe util-scrrel
           .
       call-scrrel-copia-agg-prog.
           initialize util-scrrel
           move "RILASCIA-COPIA"        to scrrel-ope
           move tm-prg                to scrrel-prg
           move "S"                     to scrrel-fl-singolo
           call "SCRREL"             using stringhe util-scrrel
           .
       call-scrrel-fine.
           initialize util-scrrel
           move "RILASCIA-FINE"         to scrrel-ope
           move ext-prg-dir             to scrrel-path-in
           move "c:\ecoge\tmp"          to scrrel-path-out
           move fl-aggiungi-files       to scrrel-fl-aggiungi
           move "S"                     to scrrel-fl-singolo
           call "SCRREL"             using stringhe util-scrrel
           cancel "SCRREL"
           .

      *---------------------------------------------------------------**
      * Azzeramento ID assoluto per il programma selezionato
      * NB: Questa operazione non va fatta se il programma e' stato
      *    rilasciato, altrimenti le eventuali customizzazioni possono
      *    essere perse
      *---------------------------------------------------------------**
       x-azzera-id-assoluto.
           if tm-pac = " " or
              tm-prg = " "
              move "Seleziona prima pacchetto/programma!!" to wb-msg
              perform vbx-msg-error
              exit paragraph
           end-if

           move "Attenzione, questa operazione non e' consigliabile su p
      -      "rogrammi già rilasciati. Proseguo?" to wb-msg
           perform acc-conf-custom
           if not f3
              exit paragraph
           end-if

           initialize fld-rec
           move tm-pac                  to fld-pac
           move tm-prg                  to fld-prg
           perform st-scrfield-notmin
           if w-verbo-ok
              perform with test before until w-fine-file
                 perform rdnxt-scrfield
                 if w-verbo-ok and 
                    fld-pac = tm-pac and
                    fld-prg = tm-prg

                    move 0              to fld-id-assoluto
                    perform rwr-scrfield

                    initialize fldx-rec
                    move fld-chia1      to fldx-chia1
                    perform rd-scrfldex
                    if w-verbo-ok
                       perform del-scrfldex
                    end-if
                  else
                    move k-fine-file   to statusfi
                 end-if
              end-perform
           end-if
           move k-verbo-ok             to statusfi

           


           move "Operazione terminata" to wb-msg
           perform vbx-msg-info



           .

      *---------------------------------------------------------------**
      * Rigenerazione totale maschere
      * Per ciascun programma definito nella base dati, per il pacchetto 
      *    corrente, rigenero tutti i programmi
      * NB: rigenero solo i programmi presenti in release
      *---------------------------------------------------------------**
       x-rigenerazione-totale-screens.
           if tm-pac = " "
              move "Seleziona prima il pacchetto!!" to wb-msg
              perform vbx-msg-error
              exit paragraph
           end-if

           move "ATTENZIONE: eseguo la rigenerazione di tutti i programm
      -      "i del pacchetto?"         to wb-msg
           perform acc-conf-custom
           if not f3
              exit paragraph
           end-if

           move "N"                     to x-rigenera-risorse
           move "Eseguo anche la generazione risorse?" to wb-msg
           perform acc-conf-custom
           if f3
              move "S"                  to x-rigenera-risorse
           end-if

           move 0                       to x-prg-rigenerati

           move "Attendere prego..."    to wb-g1
           move "Programmi rigenerati: "   to wb-g2
           perform clock-msg

           initialize scr-rec
           move tm-pac                  to scr-pac
           perform st-scrforms-notmin
           if w-verbo-ok
              perform with test before until w-fine-file
                 perform rdnxt-scrforms
                 if w-verbo-ok and 
                    scr-pac = tm-pac

                    initialize prg-rec
                    move scr-pac        to prg-pack
                    move scr-prg        to prg-prg
                    move "CBL"          to prg-ext
                    perform rd-relprog
                    if w-verbo-invalido
                       exit perform cycle
                    end-if

                    add 1               to x-prg-rigenerati
                    move x-prg-rigenerati  to w-9-1
                    if w-9-1 = 0
                       accept poscur       line 1 col 1 
                                    before time 100
                       if wesc
                          move "Vuoi interrompere la rigenerazione?"
                                        to wb-msg
                          perform acc-conf-custom
                          if f3
                             perform rem-clk
                             exit paragraph
                          end-if
                       end-if
                    end-if
                    move x-prg-rigenerati  to zeta8a
                    move " "            to wb-g2
                    string "Rigenerati: " zeta8a " (" delimited size
                       scr-prg                delimited "  "
                       ")"                    delimited size
                                         into wb-g2
                    perform view-clk

                    initialize util-scrsim
                    move "RIGENERA-SILENT"  to scrsim-ope
                    move "SCRSIM-CALLED" to scrsim-called
                    move scr-pac         to scrsim-pac
                    move scr-prg         to scrsim-prg
                    move scr-frm         to scrsim-frm
                    move 0               to scrsim-pag
                    call "SCRSIM"     using stringhe util-scrsim
                    cancel "SCRSIM"

                    if x-rigenera-risorse = 'S'
                       move scr-prg     to x-prg
                       move "S"         to x-silent-mode
                       perform x-call-generazione-nav
                    end-if
                  else
                    move k-fine-file   to statusfi
                 end-if
              end-perform
           end-if
           move k-verbo-ok             to statusfi

           perform rem-clk

           cancel "SCRSIM"
           .

      *---------------------------------------------------------------**
      * Creazione menu principale
      *---------------------------------------------------------------**
       x-crea-menu.
      *---------------------------------------------------------------**
      * Creo l'handle principale
      *---------------------------------------------------------------**
           call "W$MENU" using wmenu-new,
                        giving thm-main

      *---------------------------------------------------------------**
      * Creo il menu "File"
      *---------------------------------------------------------------**
           call "W$MENU" using wmenu-new,
                        giving thm-file
      *---------------------------------------------------------------**
      * Appendo le voci di menu che mi interessano al menu "File"
      *---------------------------------------------------------------**
           call "W$MENU" using wmenu-add, thm-file, 0, 0,
                               "&Esci", k-fun-wesc, 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-file, 0, w-separator,
                               " ", 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-file, 0, 0,
                               "&Azzera ID assoluto",
                               k-menu-id-azzera-id-assoluto, 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-file, 0, w-separator,
                               " ", 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-file, 0, 0,
                               "&Rigenera tutto",
                               k-menu-id-rigenera-tutto, 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-file, 0,
                               w-separator, " ", 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-file, 0, 0,
                "Rilascio spot", k-menu-id-rilascio-spot, 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-file, 0,
                               w-separator, " ", 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-file, 0, 0,
                "Genera risorse", k-menu-id-generazione-nav, 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-file, 0, 0,
                "Navigazione", k-menu-id-navigazione, 0
                                  giving myResult
      *---------------------------------------------------------------**
      * Aggancio il menu "File" al menu principale
      *---------------------------------------------------------------**
           call "W$MENU" using wmenu-add, thm-main, 0, 0,
                               "&File", 0, thm-file
                                  giving myResult

      *---------------------------------------------------------------**
      * Creo il menu "Lingua"
      *---------------------------------------------------------------**
           call "W$MENU" using wmenu-new,
                        giving thm-lingua
      *---------------------------------------------------------------**
      * Appendo le voci di menu che mi interessano al menu "Lingua"
      *---------------------------------------------------------------**
           call "W$MENU" using wmenu-add, thm-lingua, 0, 0,
                "&Esportazione", k-menu-id-esportazione, 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-lingua, 0, 0,
                "&Importazione", k-menu-id-importa-testi, 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-lingua, 0,
                               w-separator, " ", 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-lingua, 0, 0,
                "&Tabella lingue", k-menu-id-tabella-lingue, 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-lingua, 0,
                               w-separator, " ", 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-lingua, 0, 0,
                "Testi &Generali", k-menu-id-testi-generali, 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-lingua, 0, 0,
                "Testi &Programma", k-menu-id-testi-programma, 0
                                  giving myResult
      *---------------------------------------------------------------**
      * Aggancio il menu "Lingua" al menu principale
      *---------------------------------------------------------------**
           call "W$MENU" using wmenu-add, thm-main, 0, 0,
                               "&Lingua", 0, thm-lingua
                                  giving myResult

      *---------------------------------------------------------------**
      * Creo il menu "DB"
      *---------------------------------------------------------------**
           call "W$MENU" using wmenu-new,
                        giving thm-db
      *---------------------------------------------------------------**
      * Appendo le voci di menu che mi interessano al menu "Lingua"
      *---------------------------------------------------------------**
           call "W$MENU" using wmenu-add, thm-db, 0, 0,
                "&Tabelle", k-menu-id-tabelle, 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-db, 0, 0,
                "T&emplate", k-menu-id-template, 0
                                  giving myResult
           call "W$MENU" using wmenu-add, thm-db, 0, 0,
                "&Dipendenze", k-menu-id-dipendenze, 0
                                  giving myResult
      *---------------------------------------------------------------**
      * Aggancio il menu "DB" al menu principale
      *---------------------------------------------------------------**
           call "W$MENU" using wmenu-add, thm-main, 0, 0,
                               "&DB", 0, thm-db
                                  giving myResult



      *---------------------------------------------------------------**
      * Creo il menu "?"
      * Appendo le voci di menu che mi interessano al menu "?"
      * Aggancio il menu "Help" al menu principale
      *---------------------------------------------------------------**
           call "W$MENU" using wmenu-new,
                        giving thm-help

           call "W$MENU" using wmenu-add, thm-help, 0, 0,
                               "&Aiuto", k-fun-ctrl-f1, 0
                                  giving myResult

           call "W$MENU" using wmenu-add, thm-main, 0, 0,
                               "&?", 0, thm-help
                                  giving myResult
           .
       x-destroy-menu.
           if thm-main <> 0
              call "W$MENU" using wmenu-destroy, thm-main
                                  giving myResult
           end-if
           .

      **---------------------------------------------------------------**
      ** Carico scrfield x modificarlo
      **---------------------------------------------------------------**
       x-leggi-scrfield-x-modifica.
           initialize fld-rec
           move tm-pac                  to fld-pac   
           move tm-prg                  to fld-prg   
           move tm-frm                  to fld-frm   
           move tm-pag                  to fld-pag   
           move tm-prog (i)             to fld-prog
           perform rd-scrfield-lk
           .

      **---------------------------------------------------------------**
      ** Controlli necessari prima di passare alla parte di dettaglio 
      **---------------------------------------------------------------**
       x-imposta-conferma-pagina.
           move "S"                to x-f3-ok
           move spaces             to x-f3-msg
           perform x-controlla-tm-pac
           if x-f3-ok-parziale = "N"
              move "N"             to x-f3-ok
           end-if
           perform x-controlla-tm-prg
           if x-f3-ok-parziale = "N"
              move "N"             to x-f3-ok
           end-if
           perform x-controlla-tm-frm
           if x-f3-ok-parziale = "N"
              move "N"             to x-f3-ok
           end-if
           perform x-controlla-tm-pag
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           if x-f3-ok = "S"
              modify handle-status-bar,
                panel-index = 1,
                panel-text = "Dati corretti"
            else
              modify handle-status-bar,
                panel-index = 1,
                panel-text = " "
           end-if
           .

      *******************************************************************
      * Qui imposto il pulsante di conferma se ho tutti i requisiti     *
      * necessari                                                       *
      *******************************************************************
       x-imposta-conferma.
           move "S"                to x-f3-ok
           move spaces             to x-f3-msg
           perform x-controlla-tm-pac
           if x-f3-ok-parziale = "N"
              move "N"             to x-f3-ok
           end-if
           perform x-controlla-tm-prg
           if x-f3-ok-parziale = "N"
              move "N"             to x-f3-ok
           end-if
           perform x-controlla-tm-frm
           if x-f3-ok-parziale = "N"
              move "N"             to x-f3-ok
           end-if
           perform x-controlla-tm-tml
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-tml-des
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-des
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-tip
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-check-true
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-check-false
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-entry-point
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-grid-dinamica
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-edit-grid
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-color-form
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-sezione
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-pb-bitmap
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-size
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-size-dec
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-v-size
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-h-size
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-v-pos
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-h-pos
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-id
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-nome
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-value
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-indice
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-enabled
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-visible
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-align
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-case 
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-color-control
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-frame-style 
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-layout
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-css-classe
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-label
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-label-v-pos-rel
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-label-h-pos-rel
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-label-v-size
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-label-h-size
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-liv
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-color-label
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-grid-frame
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-full-height
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-notify
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-self-act
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-cent-head
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-headings
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-hnd-label
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-val-label
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-lab-def
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-s67-liv-ric
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-u10-divisa 
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-u10-data
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-u10-tipo-dato 
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-u10-edit-punti
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-s52-verifica 
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-s93-verifica 
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-status-bar
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-exception
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-evidenza
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-secure
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if
           perform x-controlla-tm-fl-pos-man
           if x-f3-ok-parziale = "N"
              move "N"                   to x-f3-ok
           end-if

      **---------------------------------------------------------------**
      ** Controlli incrociati
      **---------------------------------------------------------------**
      ** Qui controllo l'id, che e' 
      **  obbligatorio se lo stato di abilitazione del controllo
      **  e' diverso da "disabled" e il controllo non e' statico
      **--------------------------------------------------------------**
           if tm-id (i) = 0
              if tm-enabled (i) <> "0"          and
                (tm-tip (i) <> k-ctr-frame       and
                 tm-tip (i) <> k-ctr-gruppo      and
                 tm-tip (i) <> k-ctr-tab-control and
                 tm-tip (i) <> k-ctr-label    )
                 move "Se il controllo non e' disabilitato, l'id e' obbl
      -           "igatorio!!!"         to x-f3-msg
                 move "N"               to x-f3-ok
                 modify e-tm-id, color = ext-color-error
              end-if
           end-if


      *******************************************************************
      * Il tasto di aggiornamento lo lascio sempre attivo, altrimenti   *
      * potrei non rendermi conto di cosa c'e` che non va!!!            *
      *******************************************************************
           if x-f3-ok = "S"
              modify handle-status-bar,
                panel-index = 1,
                panel-text = "Dati corretti"
            else
              modify handle-status-bar,
                panel-index = 1,
                panel-text = " "
           end-if
           .
      **---------------------------------------------------------------**
      ** Controlli sui singoli campi 
      **---------------------------------------------------------------**
      ** Campi di testata
      **---------------------------------------------------------------**
       x-controlla-tm-pac.
           move "S"                      to x-f3-ok-parziale
           if tm-pac = spaces
              move "Pacchetto obbligatoria!!" to x-f3-msg
              move "N"                   to x-f3-ok-parziale
           end-if
           initialize pack-rec
           move tm-pac                   to pack-codice
           perform rd-relpacch
           if w-verbo-invalido
              move "Pacchetto non esistente!!" to x-f3-msg
              move "N"                   to x-f3-ok-parziale
           end-if
           move pack-desc                to tm-pac-des
           display e-tm-pac-des
           move tm-pac                   to dir-pack     
           move "CBL"                    to dir-ext     
           perform rd-reldirpa
           if w-verbo-invalido
              move "Direttorio di lavoro sul pacchetto non esistente!!" 
                                         to x-f3-msg
              move "N"                   to x-f3-ok-parziale
           end-if
           move spaces             to tm-pac-cbl-dir
           string dir-lav-drive ":\" dir-lav-dir 
              delimited size     into tm-pac-cbl-dir
           move tm-pac-cbl-dir          to tm-pac-cbl-dir-client
           if x-f3-ok-parziale = "S"
              modify e-tm-pac, color = ext-color-controls
            else
              modify e-tm-pac, color = ext-color-error
           end-if
           
           move tm-pac                   to dir-pack     
           move "COB"                    to dir-ext     
           perform rd-reldirpa
           if w-verbo-invalido
              move "Direttorio di lavoro sul pacchetto non esistente!!" 
                                         to x-f3-msg
              move "N"                   to x-f3-ok-parziale
           end-if
           move spaces             to tm-pac-cob-dir
           string dir-lav-drive ":\" dir-lav-dir 
              delimited size     into tm-pac-cob-dir

           if ext-thin-client = "S"
              initialize util-string
              move function lower-case(tm-pac-cbl-dir)  to us-pub-object
              move function lower-case(dir-lav-drive)   to us-pub-target
              move ":\"                  to us-pub-target (2:2)
              move "/programmi/"                        to us-pub-source
              perform string-replace
              move us-pub-object         to tm-pac-cbl-dir        
              inspect tm-pac-cbl-dir replacing all "\" by "/"

              initialize util-string
              move function lower-case(tm-pac-cob-dir)  to us-pub-object
              move function lower-case(dir-lav-drive)   to us-pub-target
              move ":\"                  to us-pub-target (2:2)
              move "/programmi/"                        to us-pub-source
              perform string-replace
              move us-pub-object         to tm-pac-cob-dir        
              inspect tm-pac-cob-dir replacing all "\" by "/"
           end-if

           move tm-pac-cbl-dir          to ext-lav-cbl-dir-server

           move spaces                   to ext-out-dir
           string tm-pac-cbl-dir            delimited " "
              ext-os-slash "screens"        delimited size 
                                       into ext-out-dir

           if x-f3-ok-parziale = "S"
              modify e-tm-pac, color = ext-color-controls
            else
              modify e-tm-pac, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Controllo programma
      ** Se esiste la form 1 prelevo da qui l'attributo "gestione lingua"
      **--------------------------------------------------------------**
       x-controlla-tm-prg.
           move "S"                      to x-f3-ok-parziale
           if tm-prg = spaces
              move "Programma obbligatorio!!" to x-f3-msg
              move "N"                   to x-f3-ok-parziale
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-prg, color = ext-color-controls
            else
              modify e-tm-prg, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Gestione lingua
      **--------------------------------------------------------------**
       x-controlla-tm-ges-lingua.
           move "S"                     to x-f3-ok-parziale
           if tm-ges-lingua not = "S" and 
              tm-ges-lingua not = "N" 
              move "Gestione lingua non definita!!" 
                                        to x-f3-msg
              move "N"                  to x-f3-ok-parziale
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-ges-lingua, color = ext-color-label
            else
              modify e-tm-ges-lingua, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Gestione customizzazioni
      **--------------------------------------------------------------**
       x-controlla-tm-ges-custom.
           move "S"                     to x-f3-ok-parziale
           if tm-ges-custom not = "S" and 
              tm-ges-custom not = "N" 
              move "Gestione customizzazioni non definita!!" 
                                        to x-f3-msg
              move "N"                  to x-f3-ok-parziale
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-ges-custom, color = ext-color-label
            else
              modify e-tm-ges-custom, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Gestione WD2
      **--------------------------------------------------------------**
       x-controlla-tm-ges-wd2.
           move "S"                     to x-f3-ok-parziale
           if tm-ges-wd2 not = "S" and 
              tm-ges-wd2 not = "N" 
              move "Gestione WD2 non definita!!" 
                                        to x-f3-msg
              move "N"                  to x-f3-ok-parziale
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-ges-wd2, color = ext-color-label
            else
              modify e-tm-ges-wd2, color = ext-color-error
           end-if
           .
       x-controlla-tm-frm.
           move "S"                      to x-f3-ok-parziale
           if tm-frm = zero
              move "Form obbligatoria!!" to x-f3-msg
              move "N"                   to x-f3-ok-parziale
           end-if
           initialize scr-rec
           move tm-pac                   to scr-pac
           move tm-prg                   to scr-prg
           move tm-frm                   to scr-frm
           perform rd-scrforms
           if not w-verbo-ok
              move "Form non esistente, vuoi inserirla?" to wb-msg
              perform acc-conf-custom
              if not f3
                 move "Form non esistente!!" to x-f3-msg
                 move "N"                to x-f3-ok-parziale
              end-if
      *       initialize tm-frm-dati
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-frm, color = ext-color-controls
            else
              modify e-tm-frm, color = ext-color-error
           end-if
           .
       x-controlla-tm-frm-des.
           move "S"                      to x-f3-ok-parziale
           if tm-frm-des = spaces
              move "Descrizione form obbligatoria" to x-f3-msg
              move "N"             to x-f3-ok-parziale
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-frm-des, color = ext-color-controls
            else
              modify e-tm-frm-des, color = ext-color-error
           end-if
           .
       x-controlla-tm-frm-v-size.
           move "S"                      to x-f3-ok-parziale
           if tm-frm-v-size = zero
              move "Altezza form obbligatoria!!" to x-f3-msg
              move "N"                   to x-f3-ok-parziale
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-frm-v-size, color = ext-color-controls
            else
              modify e-tm-frm-v-size, color = ext-color-error
           end-if
           .
       x-controlla-tm-frm-h-size.
           move "S"                      to x-f3-ok-parziale
           if tm-frm-h-size = zero
              move "Larghezza form obbligatoria!!" to x-f3-msg
              move "N"                   to x-f3-ok-parziale
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-frm-h-size, color = ext-color-controls
            else
              modify e-tm-frm-h-size, color = ext-color-error
           end-if
           .
       x-controlla-tm-pag.
           move "S"                      to x-f3-ok-parziale
           if tm-pag = zero
              move "Sezione principale"  to tm-pag-des fpg-des
            else
              initialize fpg-rec
              move tm-pac          to fpg-pac
              move tm-prg          to fpg-prg
              move tm-frm          to fpg-frm
              move tm-pag          to fpg-pag
              perform rd-scrfmpag
              if w-verbo-invalido
                 move "Pagina non esistente. Vuoi inserirla?" 
                                   to wb-msg
                 perform acc-conf-custom
                 if not f3
                    move "Pagina non esistente!!" to x-f3-msg
                    move "N"       to x-f3-ok-parziale
                 end-if
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-pag, color = ext-color-controls
            else
              modify e-tm-pag, color = ext-color-error
           end-if
           .
       x-controlla-tm-pag-des.
           move "S"                      to x-f3-ok-parziale
           if tm-pag-des = spaces
              move "Descrizione pagina obbligatoria" to x-f3-msg
              move "N"             to x-f3-ok-parziale
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-pag-des, color = ext-color-controls
            else
              modify e-tm-pag-des, color = ext-color-error
           end-if
           .
      **---------------------------------------------------------------**
      ** Campi di dettaglio campo
      **---------------------------------------------------------------**
       x-controlla-tm-tml.
           move "S"                      to x-f3-ok-parziale
           if tm-tml (i) not = spaces
              initialize ftm-rec
              move tm-tml (i)      to ftm-cod
              perform rd-scrtempl
              if w-verbo-invalido
                 move "Template non esistente, vuoi inserirlo?" 
                                   to wb-msg
                 perform acc-conf-custom
                 if not f3
                    move "Template non esistente!!" to x-f3-msg
                    move "N"       to x-f3-ok-parziale
                 end-if
               else
                 move ftm-des      to tm-tml-des (i)
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-tml, color = ext-color-controls
            else
              modify e-tm-tml, color = ext-color-error
           end-if
           .
       x-controlla-tm-tml-des.
           move "S"                to x-f3-ok-parziale
           if tm-tml-des (i) = spaces
              if x-enabled-tm-tml-des = 1
                 move "Descrizione template obbligatoria!!" 
                                   to x-f3-msg
                 move "N"          to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-tml-des, color = ext-color-controls
            else
              modify e-tm-tml-des, color = ext-color-error
           end-if
           .
       x-controlla-tm-des.
           move "S"                to x-f3-ok-parziale
           if tm-des (i) = spaces
              move "Descrizione obbligatoria!!" 
                                   to x-f3-msg
              move "N"             to x-f3-ok-parziale
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-des, color = ext-color-controls
            else
              modify e-tm-des, color = ext-color-error
           end-if
           .
       x-controlla-tm-tip.
           move "S"                to x-f3-ok-parziale
           if tm-tip (i) = spaces
              move "Tipo controllo obbligatorio!!" to x-f3-msg
              move "N"             to x-f3-ok-parziale
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-tip, color = ext-color-controls
            else
              modify e-tm-tip, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Bottone bitmap?
      **--------------------------------------------------------------**
       x-controlla-tm-pb-bitmap.
           move "S"                to x-f3-ok-parziale

           perform x-ctr-color-tm-pb-bitmap
           .
      **--------------------------------------------------------------**
      ** Valori del check
      **--------------------------------------------------------------**
       x-controlla-tm-check-true.
           move "S"                to x-f3-ok-parziale
           if tm-check-true (i) = spaces
              if x-enabled-tm-check-true = 1
                 move "Valore 'true' del check-box errati!!" to x-f3-msg
                 move "N"          to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-check-true, color = ext-color-controls
            else
              modify e-tm-check-true, color = ext-color-error
           end-if
           .
       x-controlla-tm-check-false.
           move "S"                to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-check-false, color = ext-color-controls
            else
              modify e-tm-check-false, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Dimensione caratteri e decimali
      **--------------------------------------------------------------**
       x-controlla-tm-size.
           move "S"                to x-f3-ok-parziale
           if tm-size (i) < 1    or
              tm-size (i) > 8000 
              if x-enabled-tm-size = 1
                 move "Lunghezza controllo errata!!" to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-size, color = ext-color-controls
            else
              modify e-tm-size, color = ext-color-error
           end-if
           .
       x-controlla-tm-size-dec.
           move "S"                to x-f3-ok-parziale
           if x-enabled-tm-size-dec = 1
              if tm-tip (i) = k-ctr-valore
                 if tm-size-dec (i)     > 6 and
                    tm-size-dec (i) not = 99
                    move "Numero di decimali errato!!" to x-f3-msg
                    move "N"          to x-f3-ok-parziale
                 end-if
               else
                 if tm-size-dec (i) > 6 
                    move "Numero di decimali errato!!" to x-f3-msg
                    move "N"          to x-f3-ok-parziale
                 end-if
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-size-dec, color = ext-color-controls
            else
              modify e-tm-size-dec, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Altezza in linee
      **--------------------------------------------------------------**
       x-controlla-tm-v-size.
           move "S"                to x-f3-ok-parziale
           if tm-v-size (i) = 0       
              if x-enabled-tm-v-size = 1
                 move "Altezza controllo errata!!" to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-v-size, color = ext-color-controls
            else
              modify e-tm-v-size, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** larghezza in caratteri
      ** Non obbligatoria per i check box
      **--------------------------------------------------------------**
       x-controlla-tm-h-size.
           move "S"                to x-f3-ok-parziale
           if tm-h-size (i)  = 0           and
              tm-tip (i) not = k-ctr-check
              if x-enabled-tm-h-size = 1
                 move "Larghezza controllo errata!!" to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-h-size, color = ext-color-controls
            else
              modify e-tm-h-size, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Posizione verticale controllo
      **--------------------------------------------------------------**
       x-controlla-tm-v-pos.
           move "S"                to x-f3-ok-parziale
           if tm-v-pos (i) = 0       
              if x-enabled-tm-v-pos = 1
                 move "Posizione verticale controllo errata!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-v-pos, color = ext-color-controls
            else
              modify e-tm-v-pos, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Posizione orizzontale controllo
      **--------------------------------------------------------------**
       x-controlla-tm-h-pos.
           move "S"                to x-f3-ok-parziale
           if tm-h-pos (i) = 0       
              if x-enabled-tm-h-pos = 1
                 move "Posizione orizzontale controllo errata!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-h-pos, color = ext-color-controls
            else
              modify e-tm-h-pos, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** ID controllo
      **--------------------------------------------------------------**
       x-controlla-tm-id.
           move "S"                to x-f3-ok-parziale

           if x-f3-ok-parziale = "S"
              modify e-tm-id, color = ext-color-controls
            else
              modify e-tm-id, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Nome controllo
      **--------------------------------------------------------------**
       x-controlla-tm-nome.
           move "S"                to x-f3-ok-parziale
           if tm-nome (i) = spaces 
              if tm-tip (i) not = k-ctr-frame and
                 tm-tip (i) not = k-ctr-label
                 if x-enabled-tm-nome = 1
                    move "Nome controllo non definito!!" 
                                   to x-f3-msg
                    move "N"       to x-f3-ok-parziale
                 end-if
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-nome, color = ext-color-controls
            else
              modify e-tm-nome, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Attributo "value"
      **--------------------------------------------------------------**
       x-controlla-tm-fl-value.
           move "S"                to x-f3-ok-parziale
           if tm-fl-value (i) not = "S" and 
              tm-fl-value (i) not = "N" and
              tm-fl-value (i) not = "I"
              if x-enabled-tm-fl-value = 1
                 move "Attributo 'value' non definito!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-fl-value, color = ext-color-controls
            else
              modify e-tm-fl-value, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Attributo "indice"
      **--------------------------------------------------------------**
       x-controlla-tm-indice.
           move "S"                to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-indice, color = ext-color-controls
            else
              modify e-tm-indice, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Frame associato alla griglia?
      **--------------------------------------------------------------**
       x-controlla-tm-fl-grid-frame.
           move "S"                to x-f3-ok-parziale
           if tm-fl-grid-frame (i) not = "S" and 
              tm-fl-grid-frame (i) not = "N" 
              if x-enabled-tm-fl-grid-frame = 1
                 move "Attributo 'Grid frame' non definito!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-fl-grid-frame, color = ext-color-label
            else
              modify e-tm-fl-grid-frame, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Attributo "full height"
      **--------------------------------------------------------------**
       x-controlla-tm-fl-full-height.
           move "S"                to x-f3-ok-parziale
           if tm-fl-full-height (i) not = "S" and 
              tm-fl-full-height (i) not = "N" 
              if x-enabled-tm-fl-full-height = 1
                 move "Attributo 'full height' non definito!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-fl-full-height, color = ext-color-label
            else
              modify e-tm-fl-full-height, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Attributo "in evidenza"
      **--------------------------------------------------------------**
       x-controlla-tm-fl-evidenza.
           move "S"                to x-f3-ok-parziale
           if tm-fl-evidenza (i) not = "S" and 
              tm-fl-evidenza (i) not = "N" 
              if x-enabled-tm-fl-evidenza = 1
                 move "Attributo 'In evidenza' non definito!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-fl-evidenza, color = ext-color-label
            else
              modify e-tm-fl-evidenza, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Attributo "secure"
      **--------------------------------------------------------------**
       x-controlla-tm-fl-secure.
           move "S"                to x-f3-ok-parziale
           if tm-fl-secure (i) not = "S" and 
              tm-fl-secure (i) not = "N" 
              if x-enabled-tm-fl-secure = 1
                 move "Attributo 'Secure' non definito!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-fl-secure, color = ext-color-label
            else
              modify e-tm-fl-secure, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Attributo "pos-man"
      **--------------------------------------------------------------**
       x-controlla-tm-fl-pos-man.
           move "S"                to x-f3-ok-parziale
           if tm-fl-pos-man (i) not = "S" and 
              tm-fl-pos-man (i) not = "N" 
              if x-enabled-tm-fl-pos-man = 1
                 move "Attributo 'Resize-man' non definito!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-fl-pos-man, color = ext-color-label
            else
              modify e-tm-fl-pos-man, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Attributo "enabled"
      **--------------------------------------------------------------**
       x-controlla-tm-enabled.
           move "S"                to x-f3-ok-parziale
           if tm-enabled (i) = spaces
              move "Attributo 'enabled' obbligatorio!!" to x-f3-msg
              move "N"             to x-f3-ok-parziale
           end-if
           if tm-enabled (i) = "D" 
              if tm-fl-value (i) not = "I"
                 move "Enabled 'dettaglio': il 'value' del controllo dev
      -            "e essere collegato ad indice!!" to x-f3-msg
                 move "N"          to x-f3-ok-parziale
              end-if
           end-if

           if x-f3-ok-parziale = "S"
              modify e-tm-enabled, color = ext-color-controls
            else
              modify e-tm-enabled, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Attributo "visible"
      **--------------------------------------------------------------**
       x-controlla-tm-visible.
           move "S"                to x-f3-ok-parziale
           if tm-visible (i) = spaces
              move "Attributo 'visible' obbligatorio!!" to x-f3-msg
              move "N"             to x-f3-ok-parziale
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-visible, color = ext-color-controls
            else
              modify e-tm-visible, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Attributo "align"
      **--------------------------------------------------------------**
       x-controlla-tm-align.
           move "S"                to x-f3-ok-parziale
           if tm-align (i) = spaces
              if x-enabled-tm-align = 1
                 move "Attributo 'alignment' obbligatorio!!" to x-f3-msg
                 move "N"          to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-align, color = ext-color-controls
            else
              modify e-tm-align, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Attributo "case"
      **--------------------------------------------------------------**
       x-controlla-tm-case .
           move "S"                to x-f3-ok-parziale
           if tm-case (i) = spaces
              if x-enabled-tm-case  = 1
                 move "Attributo 'case' obbligatorio!!" to x-f3-msg
                 move "N"          to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-case , color = ext-color-controls
            else
              modify e-tm-case , color = ext-color-error
           end-if
           .
      *--------------------------------------------------------------**
      * Colore controllo
      *--------------------------------------------------------------**
       x-controlla-tm-color-control.
           move "S"                to x-f3-ok-parziale

           if x-f3-ok-parziale = "S"
              modify e-tm-color-control, color = ext-color-controls
            else
              modify e-tm-color-control, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Attributo "stile frame"
      **--------------------------------------------------------------**
       x-controlla-tm-frame-style .
           move "S"                to x-f3-ok-parziale
           if tm-frame-style (i) = spaces
              if x-enabled-tm-frame-style = 1
                 move "Attributo 'stile frame' obbligatorio!!" 
                                   to x-f3-msg
                 move "N"          to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-frame-style , color = ext-color-controls
            else
              modify e-tm-frame-style , color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Layout-data
      **--------------------------------------------------------------**
       x-controlla-tm-layout.
           move "S"                to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-layout, color = ext-color-controls
            else
              modify e-tm-layout, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Classe CSS
      **--------------------------------------------------------------**
       x-controlla-tm-css-classe.
           move "S"                to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-css-classe, color = ext-color-controls
            else
              modify e-tm-css-classe, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Exception-value
      **--------------------------------------------------------------**
       x-controlla-tm-exception.
           move "S"                to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-exception, color = ext-color-controls
            else
              modify e-tm-exception, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Attributo "notify"
      **--------------------------------------------------------------**
       x-controlla-tm-fl-notify.
           move "S"                to x-f3-ok-parziale
           if tm-fl-notify (i) not = "S" and 
              tm-fl-notify (i) not = "N" 
              if x-enabled-tm-fl-notify = 1
                 move "Attributo 'notify' non definito!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-fl-notify, color = ext-color-label
            else
              modify e-tm-fl-notify, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Attributo "self-act"
      **--------------------------------------------------------------**
       x-controlla-tm-fl-self-act.
           move "S"                to x-f3-ok-parziale
           if tm-fl-self-act (i) not = "S" and 
              tm-fl-self-act (i) not = "N" 
              if x-enabled-tm-fl-self-act = 1
                 move "Attributo 'self-act' non definito!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-fl-self-act, color = ext-color-label
            else
              modify e-tm-fl-self-act, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Attributo "centered-headings"
      **--------------------------------------------------------------**
       x-controlla-tm-fl-cent-head.
           move "S"                to x-f3-ok-parziale
           if tm-fl-cent-head (i) not = "S" and 
              tm-fl-cent-head (i) not = "N" 
              if x-enabled-tm-fl-cent-head = 1
                 move "Attributo 'centered-headings' non definito!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-fl-cent-head, color = ext-color-label
            else
              modify e-tm-fl-cent-head, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Attributo "headings"
      **--------------------------------------------------------------**
       x-controlla-tm-fl-headings.
           move "S"                to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-fl-headings, color = ext-color-label
            else
              modify e-tm-fl-headings, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Etichetta
      **--------------------------------------------------------------**
       x-controlla-tm-label.
           move "S"                to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-label, color = ext-color-controls
            else
              modify e-tm-label, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Posizione verticale etichetta: non e` obbligatoria per i bottoni
      **--------------------------------------------------------------**
       x-controlla-tm-label-v-pos-rel.
           move "S"                to x-f3-ok-parziale
           if tm-label-v-pos-rel (i) = zero
              if tm-tip (i) not = k-ctr-button and
                 tm-tip (i) not = k-ctr-label
                 if x-enabled-tm-label-v-pos-rel = 1
                    move "Posizione verticale etichetta obbligatoria!!" 
                                      to x-f3-msg
                    move "N"             to x-f3-ok-parziale
                 end-if
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-label-v-pos-rel, color = ext-color-controls
            else
              modify e-tm-label-v-pos-rel, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Posizione orizzontale etichetta
      **--------------------------------------------------------------**
       x-controlla-tm-label-h-pos-rel.
           move "S"                to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-label-h-pos-rel, color = ext-color-controls
            else
              modify e-tm-label-h-pos-rel, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Dimensione verticale etichetta
      **--------------------------------------------------------------**
       x-controlla-tm-label-v-size.
           move "S"                to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-label-v-size, color = ext-color-controls
            else
              modify e-tm-label-v-size, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Dimensione orizzontale etichetta
      **--------------------------------------------------------------**
       x-controlla-tm-label-h-size.
           move "S"                to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-label-h-size, color = ext-color-controls
            else
              modify e-tm-label-h-size, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Livello in screen section
      **--------------------------------------------------------------**
       x-controlla-tm-liv.
           move "S"                to x-f3-ok-parziale
           if tm-liv (i) not numeric or 
              tm-liv (i) < "02"
              move "Livelli validi: da '02' a '50'" to x-f3-msg
              move "N"             to x-f3-ok-parziale
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-liv, color = ext-color-controls
            else
              modify e-tm-liv, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Handle sulla label?
      **--------------------------------------------------------------**
       x-controlla-tm-fl-hnd-label.
           move "S"                to x-f3-ok-parziale
           if tm-fl-hnd-label (i) not = "S" and 
              tm-fl-hnd-label (i) not = "N" 
              if x-enabled-tm-fl-hnd-label = 1
                 move "Attributo 'Handle label' non definito!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-fl-hnd-label, color = ext-color-label
            else
              modify e-tm-fl-hnd-label, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Value sulla label?
      ** Deve essere presente, se il controllo ha la visibilita` che 
      **   dipende da una variabile (se non e` un check)
      **--------------------------------------------------------------**
       x-controlla-tm-fl-val-label.
           move "S"                to x-f3-ok-parziale
           if tm-fl-val-label (i) not = "S" and 
              tm-fl-val-label (i) not = "N" 
              if x-enabled-tm-fl-val-label = 1
                 move "Attributo 'Value label' non definito!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
      *    if tm-visible (i)   = "V"         and
      *       tm-label (i) not = spaces      and
      *       tm-tip (i)   not = k-ctr-check and
      *       tm-tip (i)   not = k-ctr-merce
      *       if tm-fl-val-label (i) = "N"
      *          move "Controllo con visibilita` variabile: la label dev
      *           "e avere il 'value'!!" to x-f3-msg
      *          move "N"             to x-f3-ok-parziale
      *       end-if
      *    end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-fl-val-label, color = ext-color-label
            else
              modify e-tm-fl-val-label, color = ext-color-error
           end-if
           .
      *--------------------------------------------------------------**
      * Colore etichetta
      *--------------------------------------------------------------**
       x-controlla-tm-color-label.
           move "S"                to x-f3-ok-parziale

           if x-f3-ok-parziale = "S"
              modify e-tm-color-label, color = ext-color-controls
            else
              modify e-tm-color-label, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Nome label generica: non ci sono controlli
      **--------------------------------------------------------------**
       x-controlla-tm-lab-def.
           move "S"                to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-lab-def, color = ext-color-controls
            else
              modify e-tm-lab-def, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Definisco l'entry point sulla gestione?
      **--------------------------------------------------------------**
       x-controlla-tm-fl-entry-point.
           move "S"                to x-f3-ok-parziale
           if tm-fl-entry-point (i) not = "S" and 
              tm-fl-entry-point (i) not = "N" 
              if x-enabled-tm-fl-entry-point = 1
                 move "Attributo 'entry point' non definito!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-fl-entry-point, color = ext-color-label
            else
              modify e-tm-fl-entry-point, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Definisco se la griglia deve essere dinamica
      **--------------------------------------------------------------**
       x-controlla-tm-fl-grid-dinamica.
           move "S"                to x-f3-ok-parziale
           if tm-fl-grid-dinamica (i) not = "S" and 
              tm-fl-grid-dinamica (i) not = "N" 
              if x-enabled-tm-fl-grid-dinamica = 1
                 move "Attributo 'Auto-resize' non definito!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-fl-grid-dinamica, color = ext-color-label
            else
              modify e-tm-fl-grid-dinamica, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Definisco se la griglia e` modificabile
      ** Per il momento, se attivo questo parametro devo indicare anche
      **  l'indice, cosi` non devo modificare ulteriormente SCRSIM
      **--------------------------------------------------------------**
       x-controlla-tm-fl-edit-grid.
           move "S"                to x-f3-ok-parziale

           if tm-fl-edit-grid (i) not = "S" and 
              tm-fl-edit-grid (i) not = "A" and
              tm-fl-edit-grid (i) not = "N" 
              if x-enabled-tm-fl-edit-grid = 1
                 move "Attributo 'Griglia modificabile' non definito!!" 
                                   to x-f3-msg
                 move "N"          to x-f3-ok-parziale
              end-if
           end-if
           if tm-fl-edit-grid (i) = "S" or
              tm-fl-edit-grid (i) = "A"
              if tm-indice (i) = spaces
                 move "Se la griglia e' modificabile, devi indicare anch
      -            "e l'indice!!"  to x-f3-msg
                 move "N"          to x-f3-ok-parziale
              end-if
           end-if

           if x-f3-ok-parziale = "S"
              modify e-tm-fl-edit-grid, color = ext-color-label
            else
              modify e-tm-fl-edit-grid, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Forzo ext-color-controls?
      **--------------------------------------------------------------**
       x-controlla-tm-fl-color-form.
           move "S"                to x-f3-ok-parziale
           if tm-fl-color-form (i) not = "S" and 
              tm-fl-color-form (i) not = "N" 
              if x-enabled-tm-fl-color-form = 1
                 move "Attributo 'color-controls' non definito!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-fl-color-form, color = ext-color-label
            else
              modify e-tm-fl-color-form, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Inizio nuova sezione?
      **--------------------------------------------------------------**
       x-controlla-tm-fl-sezione.
           move "S"                to x-f3-ok-parziale
           if tm-fl-sezione (i) not = "S" and 
              tm-fl-sezione (i) not = "N" 
              if x-enabled-tm-fl-sezione = 1
                 move "Attributo 'nuova sezione' non definito!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-fl-sezione, color = ext-color-label
            else
              modify e-tm-fl-sezione, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Livello di ricerca (S67-LIV-RICERCA)
      **--------------------------------------------------------------**
       x-controlla-tm-s67-liv-ric.
           move "S"                to x-f3-ok-parziale
           if tm-s67-liv-ric (i) < 1 or
              tm-s67-liv-ric (i) > 3
              if x-enabled-tm-s67-liv-ric = 1
                 move "Valore S67-LIV-RICERCA errato!!" to x-f3-msg
                 move "N"          to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-s67-liv-ric, color = ext-color-controls
            else
              modify e-tm-s67-liv-ric, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** U10-DIVISA
      **--------------------------------------------------------------**
       x-controlla-tm-u10-divisa .
           move "S"                to x-f3-ok-parziale
           if tm-u10-divisa (i) = spaces
              if x-enabled-tm-u10-divisa  = 1
                 move "Valore U10-DIVISA errato!!" to x-f3-msg
                 move "N"          to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-u10-divisa , color = ext-color-controls
            else
              modify e-tm-u10-divisa , color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** U10-DATA
      **--------------------------------------------------------------**
       x-controlla-tm-u10-data.
           move "S"                to x-f3-ok-parziale
           if tm-u10-data (i) = spaces
              if x-enabled-tm-u10-data = 1
                 move "Valore U10-DATA errato!!" to x-f3-msg
                 move "N"          to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-u10-data, color = ext-color-controls
            else
              modify e-tm-u10-data, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** U10-TIPO-DATO
      **--------------------------------------------------------------**
       x-controlla-tm-u10-tipo-dato .
           move "S"                to x-f3-ok-parziale
           if tm-u10-tipo-dato (i) = spaces
              if x-enabled-tm-u10-tipo-dato = 1
                 move "Attributo 'U10-TIPO-DATO' obbligatorio!!" 
                                   to x-f3-msg
                 move "N"          to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-u10-tipo-dato , color = ext-color-controls
            else
              modify e-tm-u10-tipo-dato , color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Attributo "value"
      **--------------------------------------------------------------**
       x-controlla-tm-u10-edit-punti.
           move "S"                to x-f3-ok-parziale
           if tm-u10-edit-punti (i) not = "S" and 
              tm-u10-edit-punti (i) not = "N" 
              if x-enabled-tm-u10-edit-punti = 1
                 move "Attributo 'u10-edit-punti' non definito!!" 
                                   to x-f3-msg
                 move "N"             to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-u10-edit-punti, color = ext-color-label
            else
              modify e-tm-u10-edit-punti, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** S52-VERIFICA
      **--------------------------------------------------------------**
       x-controlla-tm-s52-verifica .
           move "S"                to x-f3-ok-parziale
           if tm-s52-verifica (i) = spaces
              if x-enabled-tm-s52-verifica = 1
                 move "Attributo 'S52-VERIFICA' obbligatorio!!" 
                                   to x-f3-msg
                 move "N"          to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-s52-verifica , color = ext-color-controls
            else
              modify e-tm-s52-verifica , color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** S93-VERIFICA
      **--------------------------------------------------------------**
       x-controlla-tm-s93-verifica .
           move "S"                to x-f3-ok-parziale
           if tm-s93-verifica (i) = spaces
              if x-enabled-tm-s93-verifica = 1
                 move "Attributo 'S93-VERIFICA' obbligatorio!!" 
                                   to x-f3-msg
                 move "N"          to x-f3-ok-parziale
              end-if
           end-if
           if x-f3-ok-parziale = "S"
              modify e-tm-s93-verifica , color = ext-color-controls
            else
              modify e-tm-s93-verifica , color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** COntenuto della status bar
      **--------------------------------------------------------------**
       x-controlla-tm-status-bar.
           move "S"                to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-status-bar, color = ext-color-controls
            else
              modify e-tm-status-bar, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Condizione di abilitazione campo
      **--------------------------------------------------------------**
       x-controlla-tm-cond-enabled.
           move "S"                      to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-cond-enabled, color = ext-color-controls
            else
              modify e-tm-cond-enabled, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Condizione di visibilita` campo
      **--------------------------------------------------------------**
       x-controlla-tm-cond-visible.
           move "S"                      to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-cond-visible, color = ext-color-controls
            else
              modify e-tm-cond-visible, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Condizione di abilitazione campo
      **--------------------------------------------------------------**
       x-controlla-tm-cond-row-color.
           move "S"                      to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-cond-row-color, color = ext-color-controls
            else
              modify e-tm-cond-row-color, color = ext-color-error
           end-if
           .
       x-controlla-tm-gor-alt-key.
           move "S"                      to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-gor-alt-key, color = ext-color-controls
            else
              modify e-tm-gor-alt-key, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Condizione di abilitazione campo
      **--------------------------------------------------------------**
       x-controlla-tm-grid-prf-col.
           move "S"                      to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-grid-prf-col, color = ext-color-controls
            else
              modify e-tm-grid-prf-col, color = ext-color-error
           end-if
           .
      **--------------------------------------------------------------**
      ** Livello gruppo dati griglia
      **--------------------------------------------------------------**
       x-controlla-tm-grid-max-row.
           move "S"                      to x-f3-ok-parziale
           if x-f3-ok-parziale = "S"
              modify e-tm-grid-max-row, color = ext-color-controls
            else
              modify e-tm-grid-max-row, color = ext-color-error
           end-if
           .

      *******************************************************************
      * Gestione degli eventi form                                      *
      *******************************************************************
       z-99-event-form.
           perform z-99-event-form-1.
       z-99-event-form-1.
           evaluate event-type
            when cmd-tabchanged
              if fl-sono-su-grid = "S"
                 perform z-set-current-page
                 exit paragraph
              end-if
              perform z-set-current-page
            when ntf-resized
              compute w-window-lines = event-data-1 / 100
              compute w-window-size = event-data-2 / 100
              perform x-ricalcola-griglia
           end-evaluate
           .

       z-set-current-page.
           move zero    to w-tab-pag-visible (w-tab-current-page)
           evaluate w-tab-current-page
            when 1
              display s-mm-1-tab-01
            when 2
              display s-mm-1-tab-02
            when 3
              display s-mm-1-tab-03
            when 4
              display s-mm-1-tab-04
           end-evaluate
           initialize w-tab-pagine
           move 1               to w-tab-pag-visible (event-data-1)
           move event-data-1         to w-tab-current-page
           move event-data-1         to w-tab-current-page-test
           evaluate event-data-1
            when 1
              display s-mm-1-tab-01
            when 2
              display s-mm-1-tab-02
            when 3
              display s-mm-1-tab-03
            when 4
              display s-mm-1-tab-04
           end-evaluate
           .
      *******************************************************************
      * Gestione degli eventi form                                      *
      *******************************************************************
       z-99-exception-form.
           perform z-carica-controlli
      *    move "N"                      to z-exception-prosegui
      *    evaluate funzio
      *       when k-f7
      *          if w-visible-f7-comandi = 1
      *             initialize util-t28
      *             move "COGT28-CALLED" to t28-called
      *             move "Gestione"      to t28-ope
      *             move tm-tml (i)  to t28-emu
      *             call "COGT28" using stringhe
      *             cancel "COGT28"
      *          end-if
      *        when other
                  move "S"               to z-exception-prosegui
      *    end-evaluate
      *-----------------------------------------------------------------
      * Eccezioni specifiche del menu principale
      *-----------------------------------------------------------------
           evaluate funzio
            when k-menu-id-rigenera-tutto
              perform x-rigenerazione-totale-screens
              move k-exc-rimani          to z-exception-prosegui
              exit paragraph
            when k-menu-id-azzera-id-assoluto
              perform x-azzera-id-assoluto
              move k-exc-rimani          to z-exception-prosegui
              exit paragraph
            when k-menu-id-esportazione
              perform x-call-screxlng
              move k-exc-rimani          to z-exception-prosegui
              exit paragraph
            when k-menu-id-importa-testi
              perform x-call-scrimlng
              move k-exc-rimani          to z-exception-prosegui
              exit paragraph
            when k-menu-id-tabella-lingue
              perform x-call-coge0a
              move k-exc-rimani          to z-exception-prosegui
              exit paragraph
            when k-menu-id-testi-generali
              perform x-call-scrlng-testi-generali
              move k-exc-rimani          to z-exception-prosegui
              exit paragraph
            when k-menu-id-testi-programma
              perform x-call-scrlng-testi-programma
              move k-exc-rimani          to z-exception-prosegui
              exit paragraph
            when k-menu-id-rilascio-spot
              perform x-call-scrrel-rilascio-spot
              move k-exc-rimani          to z-exception-prosegui
              exit paragraph
            when k-menu-id-generazione-nav
              move tm-prg                to x-prg
              move "N"                   to x-silent-mode
              perform x-call-generazione-nav
              move k-exc-rimani          to z-exception-prosegui
              exit paragraph
            when k-menu-id-navigazione
              perform x-call-navigazione
              move k-exc-rimani          to z-exception-prosegui
              exit paragraph
            when k-menu-id-tabelle
              perform x-call-scrgfil
              move k-exc-rimani          to z-exception-prosegui
              exit paragraph
            when k-menu-id-template
              perform x-call-scrgtmpl
              move k-exc-rimani          to z-exception-prosegui
              exit paragraph
            when k-menu-id-dipendenze
              perform x-call-scrgfdip
              move k-exc-rimani          to z-exception-prosegui
              exit paragraph
           end-evaluate

           .
      *******************************************************************
      * Operazioni che devono essere eseguite dopo che nella grid viene *
      * eseguito un evento (o meglio, il cursore si sposta su una cella *
      * diversa)                                                        *
      **---------------------------------------------------------------**
      * OCCHIO all'utilizzo di FL-SONO-SU-GRID: vale "S" quando sono    *
      * gia` entrato nella grid, perche` se eseguo le istruzioni        *
      * presenti provenendo ad esempio da un controllo gestito con      *
      * COGS52 si inchioda tutto!!!                                     *
      *******************************************************************
       z-operazioni-accessorie-grid.
           evaluate event-type
            when msg-begin-entry
              inquire e-tm-tab,
                x in w-cell-x,
                entry-reason in w-entry-reason
              evaluate w-cell-x
               when tm-column-id
                 continue
               when tm-column-liv
                 continue
               when other
                 set event-action   to event-action-fail-terminate
              end-evaluate
      **------------------------------------------------------------**
      ** Dati modificati direttamente in griglia
      **------------------------------------------------------------**
            when msg-finish-entry
              inquire e-tm-tab,
                 x         in w-cell-x,
                 y         in w-cell-y,
                 cell-data in w-cell-data
              evaluate w-cell-x
      **------------------------------------------------------------**
      ** ID controllo
      **------------------------------------------------------------**
               when tm-column-id
                 inquire e-tm-tab, cell-data in tm-id (i)
                 perform x-controlla-tm-id
                 if x-f3-ok-parziale = "N"
                    move x-f3-msg            to wb-msg
                    perform vbx-msg-error
                    set event-action    to event-action-fail
                  else
                    perform x-leggi-scrfield-x-modifica
                    move tm-id (i)      to fld-id
                    perform rwr-scrfield
                 end-if
      **------------------------------------------------------------**
      ** Livello controllo
      **------------------------------------------------------------**
               when tm-column-liv
                 inquire e-tm-tab, cell-data in tm-liv (i)
                 perform x-controlla-tm-liv
                 if x-f3-ok-parziale = "N"
                    move x-f3-msg            to wb-msg
                    perform vbx-msg-error
                    set event-action    to event-action-fail
                  else
                    perform x-leggi-scrfield-x-modifica
                    move tm-liv (i)     to fld-liv
                    perform rwr-scrfield

                    move tm-liv (i)     to w-9-2
                    if tm-liv (i) numeric and
                       tm-liv (i) < 20
                       move x'0F'       to w-cell-data (1:w-9-2)
                       move tm-liv (i)  to w-cell-data (w-9-2 + 1:2)
                       modify e-tm-tab, cell-data = w-cell-data
                    end-if
                 end-if
              end-evaluate
           end-evaluate
           if fl-sono-su-grid = "S"
              perform z-carica-vari
              perform x-display-pagina
              perform z-forza-cambia-controlli
              perform x-attiva-tf-grid
           end-if
           .
      *******************************************************************
      * Chiamata per la formattazione e la definizione delle colonne    *
      *   della griglia                                                 *
      *******************************************************************
       z-prepara-tm-grid.
           set handle-grid               to handle of e-tm-tab.
           set handle-grid-appunti       to handle of e-tm-tab.

           perform x-calcola-dimensione-cella

           inquire handle-maschera,
              lines in w-window-lines,
              size  in w-window-size

           perform x-ricalcola-griglia

           modify e-tm-tab, reset-grid = 1
      *******************************************************************
      * Qui si definiscono le personalizzazione alla griglia, dipendenti*
      * da configurazioni e/o personalizzazioni                         *
      *******************************************************************
           initialize gdad-custom

           initialize util-gdad
           move "Prepara"                to gdad-ope
           move prog-err                 to gdad-prg
           move 1                        to gdad-frm
           move k-id-grid                to gdad-ctrl-id
           call "GRIDADMN" using stringhe util-gdad, null, gdad-custom
           
           .
      *******************************************************************
      * Preparazione iniziale della screen;                             *
      * - Assegno i valori possibili alle COMBO                         *
      * - Assegno i valori possibili alle LIST-BOX                      *
      *******************************************************************
       z-costruisci-controlli.
           perform z-costruisci-s60
           .


      *******************************************************************
      * Routine per assegnare un corretto valore ai controlli gestiti   *
      * attraverso subroutine (per ora CHECK e RADIO)                   *
      *******************************************************************
      * Qui vengono gestiti anche i controlli particolari per           *
      * abilitare/disabilitare i controlli                              *
      *******************************************************************
       z-carica-controlli.
           perform z-carica-vari
           perform z-forza-cambia-controlli
           .
       z-carica-vari.
           perform z-carica-s52
           perform z-carica-s68
           perform z-carica-s93
           perform z-carica-s95
           perform z-carica-u10
           .
 
      **-----------------------------------------------------------**
      ** Descrizione template: solo se il template e` presente
      **-----------------------------------------------------------**
       z-cambia-tm-tml.
           if x-enabled-tm-tml = 0
              modify e-tm-tml, color = ext-color-disabled
            else
              modify e-tm-tml, color = ext-color-controls
           end-if
           .


      **-----------------------------------------------------------**
      ** Dimensione caratteri
      **-----------------------------------------------------------**
       z-cambia-tm-size.
           if tm-tip (i) = k-ctr-alfanumerico or
              tm-tip (i) = k-ctr-numero       or
              tm-tip (i) = k-ctr-valore       or
              tm-tip (i) = k-ctr-note 
              if x-enabled-dettaglio = 1
                 if x-enabled-tm-size = 0
                    move 1         to x-enabled-tm-size
                    modify e-tm-size, enabled = x-enabled-tm-size
                 end-if
              end-if
            else
              if x-enabled-tm-size = 1
                 move 0            to x-enabled-tm-size
              end-if
              evaluate tm-tip (i)
               when k-ctr-data
                 evaluate tm-s52-verifica (i)
                  when "4" thru "5"
                     move 4        to tm-size (i)
                  when "6" thru "7"
                     move 7        to tm-size (i)
                  when "8" thru "9"
                     move 5        to tm-size (i)
                  when other
                     move 10       to tm-size (i)
                 end-evaluate
               when k-ctr-ora
                 move 5            to tm-size (i)
               when other
                 move 0            to tm-size (i)
              end-evaluate
              display e-tm-size
           end-if
           .
      **-----------------------------------------------------------**
      ** Nome controllo e attributo "value"
      **-----------------------------------------------------------**
       z-cambia-tm-fl-value.
           if tm-tip (i) = k-ctr-alfanumerico or
              tm-tip (i) = k-ctr-numero       or
              tm-tip (i) = k-ctr-valore       or
              tm-tip (i) = k-ctr-combo        or
              tm-tip (i) = k-ctr-note         or
              tm-tip (i) = k-ctr-check        or
              tm-tip (i) = k-ctr-label        or
              tm-tip (i) = k-ctr-button       or
              tm-tip (i) = k-ctr-frame        or
              tm-tip (i) = k-ctr-piano-conti  or
              tm-tip (i) = k-ctr-merce        or
              tm-tip (i) = k-ctr-data         or
              tm-tip (i) = k-ctr-ora
              if x-enabled-dettaglio = 1
                 if x-enabled-tm-fl-value = 0
                    move 1         to x-enabled-tm-fl-value
                    modify e-tm-fl-value, 
                       enabled = x-enabled-tm-fl-value
                 end-if
              end-if
            else
              if x-enabled-tm-fl-value = 1
                 move 0            to x-enabled-tm-fl-value
              end-if
              if tm-tip (i) not = " "
                 move "N"          to tm-fl-value (i)
              end-if
              display e-tm-fl-value
           end-if
           .
      **-----------------------------------------------------------**
      ** Attributo "visible"
      **-----------------------------------------------------------**
       z-cambia-tm-visible.
           if x-enabled-dettaglio = 1
              if tm-tip (i) not = k-ctr-gruppo
                 if x-enabled-tm-visible = 0
                    move 1            to x-enabled-tm-visible
                 end-if
                 modify e-tm-visible, enabled = x-enabled-tm-visible
               else
                 if x-enabled-tm-visible = 1
                    move 0            to x-enabled-tm-visible
                 end-if
                 move "1"             to tm-visible (i)
                 display e-tm-visible
              end-if
            else
              if x-enabled-tm-visible = 1
                 move 0               to x-enabled-tm-visible
              end-if
              modify e-tm-visible, enabled = x-enabled-tm-visible
           end-if
           .
      **-----------------------------------------------------------**
      ** Attributo "alignment"
      **-----------------------------------------------------------**
       z-cambia-tm-align.
           if (tm-tip (i) not = k-ctr-frame       and
               tm-tip (i) not = k-ctr-data        and
               tm-tip (i) not = k-ctr-ora         and
               tm-tip (i) not = k-ctr-grid        and
               tm-tip (i) not = k-ctr-grid-paged  and
               tm-tip (i) not = k-ctr-gruppo      and
               tm-tip (i) not = k-ctr-piano-conti and
               tm-tip (i) not = k-ctr-merce       and
      *        tm-tip (i) not = k-ctr-numero      and
               tm-tip (i) not = k-ctr-valore         ) and
              x-enabled-dettaglio = 1
              if x-enabled-tm-align = 0
                 move 1            to x-enabled-tm-align
                 modify e-tm-align, enabled = x-enabled-tm-align
              end-if
            else
              if x-enabled-tm-align = 1
                 move 0            to x-enabled-tm-align
                 modify e-tm-align, enabled = x-enabled-tm-align
              end-if
      *       if tm-tip (i) = k-ctr-numero or
      *          tm-tip (i) = k-ctr-valore
              if tm-tip (i) = k-ctr-valore
                 move "R"          to tm-align (i)
                 display e-tm-align
              end-if
              if tm-tip (i) = k-ctr-data or
                 tm-tip (i) = k-ctr-ora
                 move "L"          to tm-align (i)
                 display e-tm-align
              end-if
           end-if
           .
      **-----------------------------------------------------------**
      ** Layout-data
      **-----------------------------------------------------------**
       z-cambia-tm-layout.
           move 0                  to x-enabled-tm-layout
           if tm-tip (i) not = k-ctr-gruppo      and
              tm-tip (i) not = k-ctr-piano-conti and
              tm-tip (i) not = k-ctr-merce
              if x-enabled-dettaglio = 1
                 move 1            to x-enabled-tm-layout
              end-if
           end-if
           modify e-tm-layout, enabled = x-enabled-tm-layout
           .
      **-----------------------------------------------------------**
      ** Attributo notify-selchange
      **-----------------------------------------------------------**
       z-cambia-tm-fl-notify.
           move 0                  to x-enabled-tm-fl-notify
           if tm-tip (i) = k-ctr-combo        or
              tm-tip (i) = k-ctr-check        or
              tm-tip (i) = k-ctr-alfanumerico or
              tm-tip (i) = k-ctr-valore       or
              tm-tip (i) = k-ctr-data         or
              tm-tip (i) = k-ctr-ora          or
              tm-tip (i) = k-ctr-numero       or
              tm-tip (i) = k-ctr-note           
              if x-enabled-dettaglio = 1
                 move 1            to x-enabled-tm-fl-notify
              end-if
           end-if
           modify e-tm-fl-notify, 
              enabled = x-enabled-tm-fl-notify
           .
      **-----------------------------------------------------------**
      ** Attributo self-act: solo per i check-box
      **-----------------------------------------------------------**
       z-cambia-tm-fl-self-act.
           move 0                  to x-enabled-tm-fl-self-act
           if tm-tip (i) = k-ctr-check
              if x-enabled-dettaglio = 1
                 move 1            to x-enabled-tm-fl-self-act
              end-if
           end-if
           modify e-tm-fl-self-act, 
              enabled = x-enabled-tm-fl-self-act
           .
      **-----------------------------------------------------------**
      ** Entry point sulla gestione?
      **-----------------------------------------------------------**
       z-cambia-tm-fl-entry-point.
           if x-enabled-dettaglio = 1          and
             (tm-tip (i) = k-ctr-numero  or
              tm-tip (i) = k-ctr-data    or
              tm-tip (i) = k-ctr-ora     or
              tm-tip (i) = k-ctr-combo   or
              tm-tip (i) = k-ctr-check   or
              tm-tip (i) = k-ctr-valore  or
      *       tm-tip (i) = k-ctr-merce   or
              tm-tip (i) = k-ctr-piano-conti)
              if x-enabled-tm-fl-entry-point = 0
                 move 1            to x-enabled-tm-fl-entry-point
                 modify e-tm-fl-entry-point, 
                    enabled = x-enabled-tm-fl-entry-point
              end-if
            else
              if x-enabled-tm-fl-entry-point = 1
                 move 0            to x-enabled-tm-fl-entry-point
                 move "N"          to tm-fl-entry-point (i)
                 modify e-tm-fl-entry-point, 
                    value   = 0,
                    enabled = x-enabled-tm-fl-entry-point 
              end-if
           end-if
           .
      **-----------------------------------------------------------**
      ** Forzo ext-color-controls?
      **-----------------------------------------------------------**
       z-cambia-tm-fl-color-form.
           if x-enabled-dettaglio = 1          and
              tm-tip (i) = k-ctr-label
              if x-enabled-tm-fl-color-form = 0
                 move 1            to x-enabled-tm-fl-color-form
                 modify e-tm-fl-color-form, 
                    enabled = x-enabled-tm-fl-color-form
              end-if
            else
              if x-enabled-tm-fl-color-form = 1
                 move 0            to x-enabled-tm-fl-color-form
                 move "N"          to tm-fl-color-form (i)
                 modify e-tm-fl-color-form, 
                    value   = 0,
                    enabled = x-enabled-tm-fl-color-form 
              end-if
           end-if
           .
      **-----------------------------------------------------------**
      ** Nome label generica: e` abilitata sempre
      **-----------------------------------------------------------**
       z-cambia-tm-lab-def.
           move 0                  to x-enabled-tm-lab-def
           if x-enabled-dettaglio = 1
              move 1               to x-enabled-tm-lab-def
           end-if
           modify e-tm-lab-def, 
              enabled = x-enabled-tm-lab-def
           .
      **-----------------------------------------------------------**
      ** Etichetta
      **-----------------------------------------------------------**
      *z-cambia-tm-label.
      *    move 0                  to x-enabled-tm-label
      *    if tm-tip (i) not = k-ctr-tab-control and
      *       tm-tip (i) not = k-ctr-gruppo
      *       if x-enabled-dettaglio = 1
      *          move 1            to x-enabled-tm-label
      *       end-if
      *    end-if
      *    modify e-tm-label, enabled = x-enabled-tm-label
      *    .
      **-----------------------------------------------------------**
      ** posizione verticale etichetta
      **-----------------------------------------------------------**
       z-cambia-tm-label-v-pos-rel.
           move 0                  to x-enabled-tm-label-v-pos-rel
           if tm-label (i) not = spaces
              if tm-tip (i) not = k-ctr-check  and
      *          tm-tip (i) not = k-ctr-label  and
                 tm-tip (i) not = k-ctr-frame  and
                 tm-tip (i) not = k-ctr-gruppo
                 if x-enabled-dettaglio = 1
                    move 1         to x-enabled-tm-label-v-pos-rel
                 end-if
              end-if
           end-if
           modify e-tm-label-v-pos-rel, 
              enabled = x-enabled-tm-label-v-pos-rel
           .
      **-----------------------------------------------------------**
      ** posizione orizzontale etichetta
      **-----------------------------------------------------------**
       z-cambia-tm-label-h-pos-rel.
           move 0                  to x-enabled-tm-label-h-pos-rel
           if tm-label (i) not = spaces
              if tm-tip (i) not = k-ctr-check  and
                 tm-tip (i) not = k-ctr-frame  and
                 tm-tip (i) not = k-ctr-gruppo
                 if x-enabled-dettaglio = 1
                    move 1         to x-enabled-tm-label-h-pos-rel
                 end-if
              end-if
           end-if
           modify e-tm-label-h-pos-rel, 
              enabled = x-enabled-tm-label-h-pos-rel
           .
      **-----------------------------------------------------------**
      ** Dimensione verticale etichetta
      **-----------------------------------------------------------**
       z-cambia-tm-label-v-size.
           move 0                  to x-enabled-tm-label-v-size
           if tm-label (i) not = spaces
              if tm-tip (i) not = k-ctr-check  and
                 tm-tip (i) not = k-ctr-label  and
                 tm-tip (i) not = k-ctr-frame  and
                 tm-tip (i) not = k-ctr-gruppo
                 if x-enabled-dettaglio = 1
                    move 1         to x-enabled-tm-label-v-size
                 end-if
              end-if
           end-if
           modify e-tm-label-v-size, 
              enabled = x-enabled-tm-label-v-size
           .
      **-----------------------------------------------------------**
      ** Dimensione verticale etichetta
      **-----------------------------------------------------------**
       z-cambia-tm-label-h-size.
           move 0                  to x-enabled-tm-label-h-size
           if tm-label (i) not = spaces
              if tm-tip (i) not = k-ctr-check  and
                 tm-tip (i) not = k-ctr-label  and
                 tm-tip (i) not = k-ctr-frame  and
                 tm-tip (i) not = k-ctr-gruppo
                 if x-enabled-dettaglio = 1
                    move 1         to x-enabled-tm-label-h-size
                 end-if
              end-if
           end-if
           modify e-tm-label-h-size, 
              enabled = x-enabled-tm-label-h-size
           .
      **-----------------------------------------------------------**
      ** Handle su label?
      **-----------------------------------------------------------**
       z-cambia-tm-fl-hnd-label.
           move 0                  to x-enabled-tm-fl-hnd-label
           if tm-label (i) not = spaces
              if tm-tip (i) not = k-ctr-check  and
                 tm-tip (i) not = k-ctr-label  and
                 tm-tip (i) not = k-ctr-frame  and
                 tm-tip (i) not = k-ctr-gruppo
                 if x-enabled-dettaglio = 1
                    move 1         to x-enabled-tm-fl-hnd-label
                 end-if
              end-if
           end-if
           modify e-tm-fl-hnd-label, 
              enabled = x-enabled-tm-fl-hnd-label
           .
      **-----------------------------------------------------------**
      ** Value su label?
      **-----------------------------------------------------------**
       z-cambia-tm-fl-val-label.
           if tm-label (i)    not = spaces and
              tm-fl-hnd-label (i) = "S"    and
              tm-tip (i) not = k-ctr-check  and
              tm-tip (i) not = k-ctr-label  and
              tm-tip (i) not = k-ctr-frame  and
              tm-tip (i) not = k-ctr-gruppo and
              x-enabled-dettaglio = 1
              if x-enabled-tm-fl-val-label = 0
                 move 1            to x-enabled-tm-fl-val-label
                 modify e-tm-fl-val-label, 
                    enabled = x-enabled-tm-fl-val-label
              end-if
            else
              if x-enabled-tm-fl-val-label = 1
                 move 0            to x-enabled-tm-fl-val-label
                 move "N"          to tm-fl-val-label (i)
                 modify e-tm-fl-val-label, 
                    value   = 0,
                    enabled = x-enabled-tm-fl-val-label 
              end-if
           end-if
           .
      **-----------------------------------------------------------**
      ** Livello di ricerca (S67-LIV-RICERCA)
      **-----------------------------------------------------------**
       z-cambia-tm-s67-liv-ric.
           if tm-tip (i)          = k-ctr-piano-conti and
              x-enabled-dettaglio = 1
              if x-enabled-tm-s67-liv-ric = 0
                 move 1            to x-enabled-tm-s67-liv-ric
                 modify e-tm-s67-liv-ric, 
                    enabled = x-enabled-tm-s67-liv-ric
                 modify e-label-tm-s67-liv-ric, 
                    enabled = x-enabled-tm-s67-liv-ric
              end-if
            else
              if x-enabled-tm-s67-liv-ric = 1
                 move 0            to x-enabled-tm-s67-liv-ric
                 initialize tm-s67-liv-ric (i)
                 display e-tm-s67-liv-ric
                 display e-label-tm-s67-liv-ric
              end-if
           end-if
           .
      **-----------------------------------------------------------**
      ** U10-DIVISA
      **-----------------------------------------------------------**
       z-cambia-tm-u10-divisa.
           if tm-tip (i)          = k-ctr-valore and
              x-enabled-dettaglio = 1
              if x-enabled-tm-u10-divisa  = 0
                 move 1            to x-enabled-tm-u10-divisa 
                 modify e-label-tm-u10-divisa , 
                    enabled = x-enabled-tm-u10-divisa 
                 modify e-tm-u10-divisa , 
                    enabled = x-enabled-tm-u10-divisa 
              end-if
            else
              if x-enabled-tm-u10-divisa  = 1
                 move 0            to x-enabled-tm-u10-divisa 
                 initialize tm-u10-divisa (i)
                 display e-label-tm-u10-divisa 
                 display e-tm-u10-divisa 
              end-if
           end-if
           .
      **-----------------------------------------------------------**
      ** U10-DATA
      **-----------------------------------------------------------**
       z-cambia-tm-u10-data.
           if tm-tip (i)          = k-ctr-valore and
              x-enabled-dettaglio = 1
              if x-enabled-tm-u10-data = 0
                 move 1            to x-enabled-tm-u10-data
                 modify e-label-tm-u10-data, 
                    enabled = x-enabled-tm-u10-data
                 modify e-tm-u10-data, 
                    enabled = x-enabled-tm-u10-data
              end-if
            else
              if x-enabled-tm-u10-data = 1
                 move 0            to x-enabled-tm-u10-data
                 initialize tm-u10-data (i)
                 display e-label-tm-u10-data
                 display e-tm-u10-data
              end-if
           end-if
           .
      **-----------------------------------------------------------**
      ** U10-TIPO-DATO
      **-----------------------------------------------------------**
       z-cambia-tm-u10-tipo-dato.
           if tm-tip (i)          = k-ctr-valore and
              x-enabled-dettaglio = 1
              if x-enabled-tm-u10-tipo-dato = 0
                 move 1            to x-enabled-tm-u10-tipo-dato
                 modify e-label-tm-u10-tipo-dato, 
                    enabled = x-enabled-tm-u10-tipo-dato
                 modify e-tm-u10-tipo-dato, 
                    enabled = x-enabled-tm-u10-tipo-dato
              end-if
            else
              if x-enabled-tm-u10-tipo-dato = 1
                 move 0            to x-enabled-tm-u10-tipo-dato
                 initialize tm-u10-tipo-dato (i)
                 display e-label-tm-u10-tipo-dato
                 display e-tm-u10-tipo-dato
              end-if
           end-if
           .
      **-----------------------------------------------------------**
      ** S52-VERIFICA
      **-----------------------------------------------------------**
       z-cambia-tm-s52-verifica.
           if tm-tip (i)          = k-ctr-data and
              x-enabled-dettaglio = 1
              if x-enabled-tm-s52-verifica = 0
                 move 1            to x-enabled-tm-s52-verifica
                 modify e-label-tm-s52-verifica, 
                    enabled = x-enabled-tm-s52-verifica
                 modify e-tm-s52-verifica, 
                    enabled = x-enabled-tm-s52-verifica
              end-if
            else
              if x-enabled-tm-s52-verifica = 1
                 initialize tm-s52-verifica (i)
                 move 0            to x-enabled-tm-s52-verifica
                 display e-label-tm-s52-verifica
                 display e-tm-s52-verifica
              end-if
           end-if
           .
      **-----------------------------------------------------------**
      ** S93-VERIFICA
      **-----------------------------------------------------------**
       z-cambia-tm-s93-verifica.
           if tm-tip (i)          = k-ctr-ora and
              x-enabled-dettaglio = 1
              if x-enabled-tm-s93-verifica = 0
                 move 1            to x-enabled-tm-s93-verifica
                 modify e-label-tm-s93-verifica, 
                    enabled = x-enabled-tm-s93-verifica
                 modify e-tm-s93-verifica, 
                    enabled = x-enabled-tm-s93-verifica
              end-if
            else
              if x-enabled-tm-s93-verifica = 1
                 initialize tm-s93-verifica (i)
                 move 0            to x-enabled-tm-s93-verifica
                 display e-label-tm-s93-verifica
                 display e-tm-s93-verifica
              end-if
           end-if
           .
      **-----------------------------------------------------------**
      ** Contenuto della status bar
      **-----------------------------------------------------------**
       z-cambia-tm-status-bar.
           move 0                  to x-enabled-tm-status-bar
           if tm-tip (i) not = k-ctr-gruppo and
              tm-tip (i) not = k-ctr-frame  and
              tm-tip (i) not = k-ctr-button and
              tm-tip (i) not = k-ctr-label
              if x-enabled-dettaglio = 1
                 move 1            to x-enabled-tm-status-bar
              end-if
           end-if
           modify e-tm-status-bar, enabled = x-enabled-tm-status-bar
           modify e-label-tm-status-bar, 
              enabled = x-enabled-tm-status-bar
           .

      *******************************************************************
      * Riempimento della griglia                                       *
      *******************************************************************
       z-costruisci-grid.
           modify e-tm-tab, mass-update = 1

           initialize util-gdad
           move "Svuota-Tutto"           to gdad-ope
           move prog-err                 to gdad-prg
           move 1                        to gdad-frm
           move k-id-grid                to gdad-ctrl-id
           call "GRIDADMN" using stringhe util-gdad gdad-record

           move zero                     to k
           perform tm-pnt times
              add 1                      to k
              perform x-riempi-grid-record
              move grid-record           to gdad-record

              initialize util-gdad
              move "Add-Record"          to gdad-ope
              move prog-err              to gdad-prg
              move 1                     to gdad-frm
              move k-id-grid             to gdad-ctrl-id
              call "GRIDADMN" using stringhe util-gdad gdad-record
           end-perform
           modify e-tm-tab, num-rows = tm-pnt + 1
           modify e-tm-tab, mass-update = 0
           .
      **-----------------------------------------------------------**
      ** Richiesta di parametro check-box
      **-----------------------------------------------------------**
      *z-s95-tm-fl-obb.
      *    move "S"                      to s95-when-true
      *    move "N"                      to s95-when-false
      *    move tm-fl-obb (i)            to s95-i-field
      *    set s95-handle                to handle of e-tm-fl-obb
      *    move funzio                   to s95-exception
      *    move control-id               to s95-o-control-id
      *    call "COGS95" using stringhe util-s95
      *    move s95-exception            to funzio
      *    move s95-o-control-id         to control-id
      *    move s95-o-field              to tm-fl-obb (i)
      *    .
      *
      *******************************************************************
      *Attivo e disattivo i t.f. utilizzabili sulla griglia di dettaglio*
      *******************************************************************
       x-attiva-tf-grid.
           perform x-attiva-f4-cancella
           perform x-attiva-f5-salva-come
           perform x-attiva-f7-verifica
           perform x-attiva-sf4-griglia
           move k-f2-note-campo         to w-pbc-idx
           perform x-attiva-pbc
           move k-f6-file               to w-pbc-idx
           perform x-attiva-pbc
           move k-f9-messaggi           to w-pbc-idx
           perform x-attiva-pbc
           move k-f11-msg-stampa        to w-pbc-idx
           perform x-attiva-pbc
           move k-f10-gridsist          to w-pbc-idx
           if tm-tip (i) = k-ctr-grid or
              tm-tip (i) = k-ctr-grid-paged
              perform x-attiva-pbc
            else
              perform x-disattiva-pbc
           end-if
           if a-modalita = "S"
              perform x-attiva-f3-modifica
      *       perform x-attiva-invio
           else
              if w-dc-called = "S"
                 perform x-attiva-f3-selezione
              end-if
      *       perform x-attiva-invio-modifica
              perform x-attiva-ctrl-a-aggiungi
              perform x-attiva-ctrl-e-elimina
              perform x-attiva-ctrl-i-inserisci
           end-if
           .
       x-disattiva-tf-grid.
           perform x-disattiva-f4-cancella
           perform x-disattiva-f5-salva-come
           perform x-disattiva-f7-verifica
           perform x-disattiva-sf4-griglia
           move k-f2-note-campo         to w-pbc-idx
           perform x-disattiva-pbc
           move k-f6-file               to w-pbc-idx
           perform x-disattiva-pbc
           move k-f9-messaggi           to w-pbc-idx
           perform x-disattiva-pbc
           move k-f11-msg-stampa        to w-pbc-idx
           perform x-disattiva-pbc
           move k-f10-gridsist          to w-pbc-idx
           perform x-disattiva-pbc
           if a-modalita = "S"
              perform x-disattiva-f3-modifica
      *       perform x-disattiva-invio
           else
              if w-dc-called = "S"
                 perform x-disattiva-f3-selezione
              end-if
      *       perform x-disattiva-invio-modifica
              perform x-disattiva-ctrl-a-aggiungi
              perform x-disattiva-ctrl-e-elimina
              perform x-disattiva-ctrl-i-inserisci
           end-if
           .
      *******************************************************************
      * Riempimento del record con cui aggiornare la grid               *
      *******************************************************************
       x-riempi-grid-record.
           initialize grid-record
           move tm-prog (k)              to grd-prog
           move tm-tml (k)               to grd-tml
           move tm-tml-des (k)           to grd-tml-DES
           move tm-des (k)               to grd-des
           move spaces                   to grd-tip
           move zero                     to z
           perform 18 times
              add 1                      to z
              if tm-tip (k) = v-tm-tip (z) (4:1)
                 move v-tm-tip (z) (6:14) to grd-tip
              end-if
           end-perform
           move tm-liv (k)               to w-9-2
           if tm-liv (k) numeric and
              tm-liv (k) < 20
              move x'0F'                 to grd-liv (1:w-9-2)
              move tm-liv (k)            to grd-liv (w-9-2 + 1:2)
           end-if
           if tm-nome (k) (1:1) = "%"
              string "e-" tm-nome (k) (2:29) 
                 delimited size        into grd-handle
            else
              string "e-tm-" tm-nome (k) delimited size into grd-handle
           end-if
           move tm-padre (k)             to grd-padre
           move tm-size (k)              to grd-size
           move tm-size-dec (k)          to grd-size-dec
           move tm-v-size (k)            to grd-v-size
           move tm-h-size (k)            to grd-h-size
           move tm-v-pos (k)             to grd-v-pos
           move tm-h-pos (k)             to grd-h-pos
           move tm-id (k)                to grd-id
           evaluate tm-fl-value (k) 
            when "S"            
              move "Si"                  to grd-fl-value
            when "N"            
              move "No"                  to grd-fl-value
            when "I"            
              move "Indice"              to grd-fl-value
           end-evaluate
           evaluate tm-enabled (k) 
            when "0"            
              move "0"                   to grd-enabled
            when "1"            
              move "1"                   to grd-enabled
            when "V"            
              move "Variabile"           to grd-enabled
            when "R"            
              move "Read-only"           to grd-enabled
            when "D"            
              move "Dettaglio"           to grd-enabled
           end-evaluate
           evaluate tm-visible (k) 
            when "0"            
              move "0"                   to grd-visible
            when "1"            
              move "1"                   to grd-visible
            when "V"            
              move "Variabile"           to grd-visible
           end-evaluate
           evaluate tm-align (k) 
            when "L"            
              move "Left"                to grd-align
            when "R"            
              move "Right"               to grd-align
            when "C"            
              move "Center"              to grd-align
           end-evaluate
           if tm-fl-grid-frame (k) = "S"
              move "Grid frame"          to grd-fl-grid-frame
           end-if
           if tm-fl-hnd-label (k) = "S"
              move "Hnd label"           to grd-fl-hnd-label
           end-if
           evaluate tm-case (k) 
            when "U"            
              move "Upper"               to grd-case
            when "L"            
              move "Lower"               to grd-case
            when "I"            
              move "Invariato"           to grd-case
           end-evaluate
           move tm-layout (k)            to grd-layout
           evaluate tm-frame-style (k) 
            when "E"            
              move "Engraved"            to grd-frame-style
            when "I"            
              move "Rimmed"              to grd-frame-style
            when "L"            
              move "Lowered"             to grd-frame-style
            when "R"            
              move "Raised"              to grd-frame-style
           end-evaluate
           if tm-fl-full-height (k) = "S"
              move "Full height"         to grd-fl-full-height
           end-if
           if tm-fl-notify (k) = "S"
              move "Notify"              to grd-fl-notify
           end-if
           if tm-fl-cent-head (k) = "S"
              move "Cent.Head."          to grd-fl-cent-head
           end-if
           move tm-check-true (k)        to grd-fl-check-true
           move tm-check-false (k)       to grd-fl-check-false
           if tm-fl-val-label (k) = "S"
              move "Val.Label"           to grd-fl-val-label
           end-if
           move tm-s67-liv-ric (k)       to grd-s67-liv-ric
           move tm-u10-divisa (k)        to grd-u10-divisa
           move tm-u10-data (k)          to grd-u10-data
           evaluate tm-u10-tipo-dato (k) 
            when "A"            
              move "Pr.acquisto"         to grd-u10-tipo-dato
            when "V"            
              move "Pr.vendita"          to grd-u10-tipo-dato
            when "I"            
              move "Importo"             to grd-u10-tipo-dato
           end-evaluate
           move tm-s52-verifica (k)      to grd-s52-verifica
           if tm-fl-entry-point (k) = "S"
              move "Si"                  to grd-fl-entry-point
           end-if
           move tm-s93-verifica (k)      to grd-s93-verifica
           if k not = 1
              move "Su"                  to grd-su
           end-if
           if k not = tm-pnt
              move "Giu"                 to grd-giu
           end-if
           move tm-id-assoluto (k)       to grd-id-assoluto
           move tm-hc-id (k)             to grd-hc-id
           if tm-hc-attivo (k) = "S"
              move "Si"                  to grd-hc-attivo
            else
              move "  "                  to grd-hc-attivo
           end-if
           move tm-fldx-id-controllo (k) to grd-fldx-id-controllo
           .

       x-cambia-pagina.
           if w-tab-current-page-test = 0
              modify e-tab-control, value = 1
              exit paragraph
           end-if
           move zero    to w-tab-pag-visible (w-tab-current-page)
           perform x-display-pagina
           initialize w-tab-pagine
           move 1               to w-tab-pag-visible (event-data-1)
           move event-data-1         to w-tab-current-page
           move event-data-1         to w-tab-current-page-test
           evaluate event-data-1
            when 1
              display s-mm-1-tab-01
            when 2
              display s-mm-1-tab-02
            when 3
              display s-mm-1-tab-03
            when 4
              display s-mm-1-tab-04
           end-evaluate
           .
       x-display-pagina.
           evaluate w-tab-current-page
            when 1
              display s-mm-1-tab-01
            when 2
              display s-mm-1-tab-02
            when 3
              display s-mm-1-tab-03
            when 4
              display s-mm-1-tab-04
           end-evaluate
           .

      *******************************************************************
      * Serie di controlli che servono per verificare l'eventuale       *
      * spostamento su un altro controllo con il mouse                  *
      *******************************************************************
      * In questo programma che ha un tab control, tutto dipende dalla  *
      * pagina corrente                                                 *
      *******************************************************************
       x-test-mouse.
           move "S"                      to x-spostamento-mouse

           if control-id = k-id-grid
              perform x-disattiva-tf-grid
           end-if
      *---------------------------------------------------------------
      * Dai dati di testata: posso rimanere solo li'
      *---------------------------------------------------------------
           evaluate control-id
            when 1 thru 19
              evaluate event-control-id
               when k-id-tm-pac
                 go to a-tm-pac
               when k-id-tm-prg
                 go to a-tm-prg
               when k-id-tm-ges-lingua
                 go to a-tm-ges-lingua
               when k-id-tm-ges-custom
                 go to a-tm-ges-custom
               when k-id-tm-ges-wd2
                 go to a-tm-ges-wd2
               when k-id-tm-frm
                 go to a-tm-frm
               when k-id-tm-frm-des
                 go to a-tm-frm-des
               when k-id-tm-frm-v-size
                 go to a-tm-frm-v-size
               when k-id-tm-frm-h-size
                 go to a-tm-frm-h-size
               when k-id-tm-pag
                 go to a-tm-pag
               when k-id-tm-pag-des
                 go to a-tm-pag-des
                when other
                 move 'N'            to x-spostamento-mouse
              end-evaluate
      *---------------------------------------------------------------
      * Dalla griglia: posso rimanere sulla griglia, o al massimo
      * cambiare la linguetta di dettaglio visualizzata
      *---------------------------------------------------------------
            when k-id-grid
              evaluate event-control-id
               when k-id-grid
                 go to a-grid
                when other
                 move 'N'            to x-spostamento-mouse
              end-evaluate
      *---------------------------------------------------------------
      * In tutti gli altri casi mi muovo dentro il tab
      *---------------------------------------------------------------
            when other
              evaluate w-tab-current-page-test
               when 00
                 move 'N'            to x-spostamento-mouse
               when 01
                 if event-type = cmd-tabchanged
                    go to a-pagina-01
                 end-if
                 evaluate event-control-id
                  when k-id-tm-tml
                    go to a-tm-tml
                  when k-id-tm-tml-des
                    go to a-tm-tml-des
                  when k-id-tm-des
                    go to a-tm-des
                  when k-id-tm-tip
                    go to a-tm-tip
                  when k-id-tm-pb-bitmap
                    go to a-tm-pb-bitmap
                  when k-id-tm-id
                    go to a-tm-id
                  when k-id-tm-nome
                    go to a-tm-nome
                  when k-id-tm-fl-value
                    go to a-tm-fl-value
                  when k-id-tm-indice
                    go to a-tm-indice
                  when k-id-tm-liv
                    go to a-tm-liv
                  when k-id-tm-fl-sezione
                    go to a-tm-fl-sezione
                  when k-id-tm-exception
                    go to a-tm-exception
                  when k-id-tm-status-bar
                    go to a-tm-status-bar
                  when k-id-tm-fl-notify
                    go to a-tm-fl-notify
                  when k-id-tm-fl-self-act
                    go to a-tm-fl-self-act
                  when k-id-tm-fl-entry-point
                    go to a-tm-fl-entry-point
                  when k-id-tm-check-true
                    go to a-tm-check-true
                  when k-id-tm-check-false
                    go to a-tm-check-false
                   when other
                    move 'N'            to x-spostamento-mouse
                 end-evaluate
               when 02
                 if event-type = cmd-tabchanged
                    go to a-pagina-02
                 end-if
                 evaluate event-control-id
                  when k-id-tm-size
                    go to a-tm-size
                  when k-id-tm-size-dec
                    go to a-tm-size-dec
                  when k-id-tm-v-size
                    go to a-tm-v-size
                  when k-id-tm-h-size
                    go to a-tm-h-size
                  when k-id-tm-v-pos
                    go to a-tm-v-pos
                  when k-id-tm-h-pos
                    go to a-tm-h-pos
                  when k-id-tm-align
                    go to a-tm-align
                  when k-id-tm-case
                    go to a-tm-case
                  when k-id-tm-color-control
                    go to a-tm-color-control
                  when k-id-tm-enabled
                    go to a-tm-enabled
                  when k-id-tm-cond-enabled
                    go to a-tm-cond-enabled
                  when k-id-tm-visible
                    go to a-tm-visible
                  when k-id-tm-cond-visible
                    go to a-tm-cond-visible
                  when k-id-tm-layout
                    go to a-tm-layout
                  when k-id-tm-fl-color-form
                    go to a-tm-fl-color-form
                  when k-id-tm-fl-evidenza
                    go to a-tm-fl-evidenza
                  when k-id-tm-fl-secure
                    go to a-tm-fl-secure
                  when k-id-tm-css-classe
                    go to a-tm-css-classe
                   when other
                    move 'N'            to x-spostamento-mouse
                 end-evaluate
               when 03
                 if event-type = cmd-tabchanged
                    go to a-pagina-03
                 end-if
                 evaluate event-control-id
                  when k-id-tm-lab-def
                    go to a-tm-lab-def
                  when k-id-tm-label
                    go to a-tm-label
                  when k-id-tm-label-v-pos-rel
                    go to a-tm-label-v-pos-rel
                  when k-id-tm-label-h-pos-rel
                    go to a-tm-label-h-pos-rel
                  when k-id-tm-label-v-size
                    go to a-tm-label-v-size
                  when k-id-tm-label-h-size
                    go to a-tm-label-h-size
                  when k-id-tm-fl-hnd-label
                    go to a-tm-fl-hnd-label
                  when k-id-tm-fl-val-label
                    go to a-tm-fl-val-label
                  when k-id-tm-color-label
                    go to a-tm-color-label
                  when k-id-tm-s67-liv-ric
                    go to a-tm-s67-liv-ric
                  when k-id-tm-u10-divisa
                    go to a-tm-u10-divisa
                  when k-id-tm-u10-data
                    go to a-tm-u10-data
                  when k-id-tm-u10-tipo-dato
                    go to a-tm-u10-tipo-dato
                  when k-id-tm-s52-verifica
                    go to a-tm-s52-verifica
                  when k-id-tm-s93-verifica
                    go to a-tm-s93-verifica
                  when k-id-tm-u10-edit-punti
                    go to a-tm-u10-edit-punti
                   when other
                    move 'N'            to x-spostamento-mouse
                 end-evaluate
               when 04
                 if event-type = cmd-tabchanged
                    go to a-pagina-04
                 end-if
                 evaluate event-control-id
                  when k-id-tm-fl-grid-dinamica
                    go to a-tm-fl-grid-dinamica
                  when k-id-tm-fl-edit-grid
                    go to a-tm-fl-edit-grid
                  when k-id-tm-fl-pos-man
                    go to a-tm-fl-pos-man
                  when k-id-tm-fl-headings
                    go to a-tm-fl-headings
                  when k-id-tm-fl-cent-head
                    go to a-tm-fl-cent-head
                  when k-id-tm-grid-prf-col
                    go to a-tm-grid-prf-col
                  when k-id-tm-grid-max-row
                    go to a-tm-grid-max-row
                  when k-id-tm-cond-row-color
                    go to a-tm-cond-row-color
                  when k-id-tm-grid-ctrl-a-ep
                    go to a-tm-grid-ctrl-a-ep
                  when k-id-tm-grid-be-ep
                    go to a-tm-grid-be-ep
                  when k-id-tm-disattiva-tf-ep
                    go to a-tm-disattiva-tf-ep
                  when k-id-tm-gor-alt-key
                    go to a-tm-gor-alt-key
                  when k-id-tm-frame-style
                    go to a-tm-frame-style
                  when k-id-tm-fl-grid-frame
                    go to a-tm-fl-grid-frame
                  when k-id-tm-fl-full-height
                    go to a-tm-fl-full-height
                   when other
                    move 'N'            to x-spostamento-mouse
                 end-evaluate
              end-evaluate



           end-evaluate
      
           if x-spostamento-mouse = 'N'
              move control-id to event-control-id
              go to x-test-mouse
           end-if
           .
      *
       copy "stato.cpy".
       copy "stato1.cpy".
       copy "winmsg.cpy".
       copy "grave.cpy".
       copy "util1.cpy".
       copy "mmmask.cpy".
       copy "scrfld.prc".
      *
       copy "utilgrid.cpy".
      *
       copy "scrforms.k01".
       copy "scrfmpag.k01".
       copy "scrtempl.k01".
       copy "scrfield.k01".
       copy "scrfield.k04".
       copy "scrfldvf.k01".
       copy "scrpgmsg.k01".
       copy "scrfldex.k01".
       copy "scrfiles.k01".
       copy "scrpgfil.k01".

       copy "relpacch.k01".
       copy "relprog.k01".
       copy "reldirpa.k01".

       end program.
