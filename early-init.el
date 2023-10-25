(setq vemv-default-background-color "#4D575F")

(setq default-frame-alist
      '((height . 52) ;; (frame-height)
        (width . 189) ;; (fragme-width)
        (left . 0)
        (top . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        (tool-bar-lines . 0)))

(add-to-list 'default-frame-alist
             `(background-color . ,vemv-default-background-color))

(add-to-list 'default-frame-alist '(undecorated . t))
