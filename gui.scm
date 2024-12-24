(define-module (gui)
  #:use-module (pstk)
  #:use-module (generate-template))

(tk-start)

(ttk-map-widgets 'all)
(ttk/set-theme "clam")

(tk/wm 'title tk "Gerador Agenda")
(tk/wm 'minsize tk 500 200)
(tk/bind tk '<Control-q> tk-end)

(define frame (tk 'create-widget 'frame 'padding: 20))
(tk/grid frame 'column: 0 'row: 0 'sticky: 'nwes)
(tk/grid 'columnconfigure tk 0 'weight: 1)
(tk/grid 'rowconfigure tk 0 'weight: 1)

(define file-input-frame (frame 'create-widget 'frame))
(define file-input (file-input-frame 'create-widget 'entry 'state: 'readonly))
(define (pick-file)
  (let* ((dft-filename (string-append "agenda-" (year-input 'get) ".pdf"))
         (filepath (tk/get-save-file 'initialfile: dft-filename 'defaultextension: ".pdf")))
    (unless (string-null? filepath)
      (file-input 'configure 'state: 'normal)
      (file-input 'delete 0 'end)
      (file-input 'insert 0 filepath)
      (file-input 'configure 'state: 'readonly))))
(define file-choose-button (file-input-frame 'create-widget 'button 'text: "..." 'command: pick-file 'width: 2))
(tk/bind file-input '<1> pick-file)
(tk/grid file-input file-choose-button 'sticky: 'we)
(tk/grid 'columnconfigure file-input-frame 0 'weight: 1)

(define year-input (frame 'create-widget 'spinbox 'from: 2024 'to: 2030))
(year-input 'set 2025)

(define action-frame (frame 'create-widget 'frame))
(define quit-button (action-frame 'create-widget 'button
                                  'text: "Sair"
                                  'command: tk-end))

(define (show-success msg)
  (tk/message-box 'icon: 'info 'type: 'ok 'parent: frame 'message: "Sucesso" 'detail: msg))
(define (show-error msg)
  (tk/message-box 'icon: 'error 'type: 'ok 'parent: frame 'message: "Erro" 'detail: msg))

(define (do-generate)
  (let ((filepath (file-input 'get))
        (year (string->number (year-input 'get))))
    (cond ((string-null? filepath) (show-error "Defina o local para salvar o arquivo."))
          (else (generate-template #:filepath filepath #:year year)
                (show-success "Arquivo gerado com sucesso.")))))
(define save-button (action-frame 'create-widget 'button 'text: "Gerar agenda!"
                                  'command: do-generate 'default: 'active))
(tk/bind tk '<Return> do-generate)
(tk/grid save-button quit-button 'padx: (list 0 10))

(define (label text)
  (frame 'create-widget 'label 'text: text 'anchor: 'e))
(tk/grid (label "Salvar como:") file-input-frame 'padx: 5 'pady: 5 'sticky: 'we)
(tk/grid (label "Ano:") year-input 'padx: 5 'pady: 5 'sticky: 'we)
(tk/grid 'x action-frame 'padx: 5 'pady: (list 20 0) 'sticky: 'w)
(tk/grid 'columnconfigure frame 1 'weight: 1)


(tk-event-loop)
