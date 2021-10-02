; only 2 chars ale special: " and \
(setq a  "hhh\"gggg") => "hhh\"gggg"
(setq a  "hhh\'gggg") => "hhh'gggg"
(setq a  "hhh'gggg") => "hhh'gggg"
(setq a  "hhh\tgggg") => "hhhtgggg"
(setq a  "hhh\\gggg") => "hhh\\gggg"

