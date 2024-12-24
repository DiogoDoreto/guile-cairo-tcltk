(define-module (gui)
  #:use-module (pstk)
  #:use-module (store colors)
  #:use-module (generate-template))

(tk-start)

(ttk-map-widgets 'all)
(ttk/set-theme "clam")

(tk/wm 'title tk "Gerador Agenda")
(tk/wm 'minsize tk 650 400)
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




(define color-frame (frame 'create-widget 'frame))
(define preview-canvas (color-frame 'create-widget 'canvas 'background: "#FFFFFF" 'width: 250 'height: 150))

(set-color-hex! 'border "#FF0000")
(set-color-hex! 'header-bg "#550000")
(set-color-hex! 'header-fg "#FFFFFF")
(set-color-hex! 'alt-header-bg "#FFDDDD")
(set-color-hex! 'alt-header-fg "#000000")

(define (draw-preview)
  (preview-canvas 'create 'rectangle 25 15 225 45 'outline: (get-color-hex 'border) 'fill: (get-color-hex 'header-bg))
  (preview-canvas 'create 'rectangle 25 45 225 75 'outline: (get-color-hex 'border) 'fill: (get-color-hex 'alt-header-bg))
  (let loop ((i 2))
    (preview-canvas 'create 'rectangle 25 (+ 15 (* i 30)) 225 (+ 45 (* i 30)) 'outline: (get-color-hex 'border))
    (when (< i 3) (loop (1+ i))))
  (preview-canvas 'create 'text 125 30 'anchor: 'center 'fill: (get-color-hex 'header-fg) 'text: "Segunda")
  (preview-canvas 'create 'text 125 60 'anchor: 'center 'fill: (get-color-hex 'alt-header-fg) 'text: "09:00"))
(draw-preview)
(define (pick-color var)
  (lambda ()
    (let ((color (tk/choose-color 'initialcolor: (get-color-hex var))))
      (unless (string-null? color)
        (preview-canvas 'delete 'all)
        (set-color-hex! var color)
        (draw-preview)))))

(define border-color-button (color-frame 'create-widget 'button 'text: "Borda" 'command: (pick-color 'border)))
(define headerbg-color-button (color-frame 'create-widget 'button 'text: "Fundo cabeçalho" 'command: (pick-color 'header-bg)))
(define headerfg-color-button (color-frame 'create-widget 'button 'text: "Texto cabeçalho" 'command: (pick-color 'header-fg)))
(define altheaderbg-color-button (color-frame 'create-widget 'button 'text: "Fundo sub-cabeçalho" 'command: (pick-color 'alt-header-bg)))
(define altheaderfg-color-button (color-frame 'create-widget 'button 'text: "Texto sub-cabeçalho" 'command: (pick-color 'alt-header-fg)))
(tk/grid preview-canvas 'column: 0 'row: 0 'rowspan: 5 'padx: (list 0 10))
(tk/grid border-color-button 'column: 1 'row: 0 'sticky: 'we)
(tk/grid headerbg-color-button 'column: 1 'row: 1 'sticky: 'we)
(tk/grid headerfg-color-button 'column: 1 'row: 2 'sticky: 'we)
(tk/grid altheaderbg-color-button 'column: 1 'row: 3 'sticky: 'we)
(tk/grid altheaderfg-color-button 'column: 1 'row: 4 'sticky: 'we)





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
(tk/grid (label "Cores:") color-frame 'padx: 5 'pady: 5 'sticky: 'we)
(tk/grid 'x action-frame 'padx: 5 'pady: (list 20 0) 'sticky: 'w)
(tk/grid 'columnconfigure frame 1 'weight: 1)




(tk-event-loop)
