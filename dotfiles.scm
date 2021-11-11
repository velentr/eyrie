(use-modules
  (gnu home)
  (gnu home services)
  (gnu packages)
  (gnu services)
  (guix gexp)
  (gnu home services shells))

(define (stat-is-type? path type)
  (let ((st (stat path #f)))
    (and st (eq? (stat:type st) type))))
(define (is-directory? path)
  (stat-is-type? path 'directory))
(define (is-file? path)
  (stat-is-type? path 'regular))

(define (make-use flag tst)
  (cons flag tst))
(define (use-flag use)
  (car use))
(define (use-test use)
  (cdr use))

(define use-flags
  (let ((available-use-flags
         (list
          (make-use 'always #t)
          (make-use 'documents #f)
          (make-use 'gentoo #f)    ;; FIXME
          (make-use 'nix
                    (lambda () (is-file?
                                (string-append
                                 (getenv "HOME")
                                 "/.nix-profile/etc/profile.d/nix.sh"))))
          (make-use 'not-skydio
                    (lambda () (not (is-directory? "/home/skydio"))))
          (make-use 'personal #f)  ;; FIXME
          (make-use 'skydio
                    (lambda () (is-directory? "/home/skydio"))))))
    (let ((use-exprs
           (filter (lambda (use)
                     (let ((tst (use-test use)))
                       (if (procedure? tst)
                           (tst)
                           tst)))
                   available-use-flags)))
      (map car use-exprs))))

(define (using? flag)
  (memq flag use-flags))

(define (filter-by-using modules)
  (map cdr
       (filter
        (lambda (module)
          (using? (car module)))
        modules)))

(define (flatfilter-by-using modules)
  (define (flatten so-far components)
    (if (null? components)
        so-far
        (flatten (append so-far (car components))
                 (cdr components))))
  (flatten '() (filter-by-using modules)))

(define (use-env)
  (cons
   ;; add the use flags to the env, for double-checking when we are running
   (cons "GUIX_USING" (string-append
                       "'"
                       (string-join (map symbol->string use-flags) " ")
                       "'"))
   (flatfilter-by-using
    '((always . (("EDITOR" . "vim")
                 ("HISTFILE" . "~/.histfile")
                 ("HISTSIZE" . "10000")
                 ("REPORTTIME" . "5")
                 ("SAVEHIST" . "10000")))
      (skydio . (("PYTHONDONTWRITEBYTECODE" . "1")        ;; don't clutter up aircam with .pyc
                 ("AIRCAM_ROOT" . "/home/skydio/aircam")  ;; default to aircam1
                 ("SKYCC_LOCAL_JOBS" . "10")))))))        ;; more parallelism

(define (use-packages)
  (flatfilter-by-using
   '((always . ("font-adobe-source-code-pro"
                "fontconfig"
                "glibc-locales"
                "nss-certs"
                "vim"
                "zsh"))
     (documents . ("ghostscript"
                   "texlive"))
     ;; revup needs a custom git implementation
     (not-skydio . ("git")))))

(define (git-config)
  (define (config-option option)
    (string-append "\t" (car option) " = " (cadr option)))
  (define (config-section section)
    (let ((header (string-append "[" (car section) "]"))
          (body (map config-option (cadr section))))
      (string-join (cons header body) "\n")))
  (let ((config-tree
         (flatfilter-by-using
          '((always .   (("alias"      (("locate"     "ls-files --")
                                        ("ref"        "rev-parse HEAD")
                                        ("st"         "status --short")
                                        ("today"      "diff --stat '@{00:00:00}'")
                                        ("undo"       "checkout --")
                                        ("unstage"    "reset HEAD --")))
                         ("credential" (("helper"     "cache")))
                         ("color"      (("ui"         "true")))
                         ("core"       (("autocrlf"   "input")
                                        ("pager"      "less -F -X")))
                         ("grep"       (("lineNumber" "true")))
                         ("push"       (("default"    "simple")))
                         ("pull"       (("ff"         "only")))
                         ("rebase"     (("autosquash" "1")))
                         ("user"       (("name"       "Brian Kubisiak")))))
            (personal . (("user"       (("email"      "brian@kubisiak.com")))))
            (skydio .   (("alias"      (("ac"         "log --first-parent")))
                         ("user"       (("email"      "brian.kubisiak@skydio.com")))))))))
    (string-join (map config-section config-tree) "\n")))

(define (zsh-aliases)
  (define (make-alias alias)
    (let ((name (car alias))
          (value (cadr alias)))
      (string-append "alias " name "=" value)))
  (let ((aliases
         (flatfilter-by-using
          '((always . (("bc" "'bc -lq'")
                       ("df" "'df -h'")
                       ("du" "'du -c -h'")
                       ("du1" "'du --max-depth=1'")
                       ("grep" "'grep --color=auto'")
                       ("ls" "'ls -hF --color=auto'")
                       ("path" "'echo $PATH'")
                       ("ping" "'ping -c 3'")))
            (documents . (("gsmerge" "'gs -dBATCH -dNOPAUSE -q -sDEVICE=pdfwrite -sOutputFile=temp.pdf'")
                          ("gssplit" "'gs -sDEVICE=pdfwrite -dSAFER -o temp.%d.pdf'")))
            (gentoo . (("esync" "'emerge --sync && notify-send portage-sync-complete'")
                       ("eupg" "'emerge --ask --update --deep --newuse --with-bdeps=y @world'")
                       ("esize" "'qsize -f -m | sort -nk8'")
                       ("esearch" "'emerge --search --fuzzy-search n'")))
            (skydio . (("ac" "'cd ~/aircam'")
                       ("yubact" "'ssh-add -D && ssh-add -e /usr/lib/x86_64-linux-gnu/opensc-pkcs11.so; ssh-add -s /usr/lib/x86_64-linux-gnu/opensc-pkcs11.so'")))))))
    (string-join (map make-alias aliases) "\n")))

(define (zsh-path)
  (let ((paths
         (flatfilter-by-using
          '((always . ("~/bin" "$path"))
            (skydio . ("${AIRCAM_ROOT}/build/host_aircam/bin"
                       "${AIRCAM_ROOT}/build/host_third_party/bin"))))))
    (string-append "typeset -U path\npath=(" (string-join paths " ") ")")))

(define (dotfile-service service-name file-name contents)
  (simple-service service-name
                  home-files-service-type
                  (list (list file-name
                              (plain-file file-name contents)))))

(define (sh-compound statements)
  (string-join statements " && "))

(define (zshrc-files)
  (flatfilter-by-using
   (list (cons 'always
               (list
                (plain-file "aliases.zsh" (zsh-aliases))
                (local-file "autocomplete.zsh")
                (local-file "cdhist.zsh")
                (local-file "history.zsh")
                (plain-file "path.zsh" (zsh-path))
                (local-file "prompt.zsh")
                (plain-file "window-title.zsh"
                            (sh-compound
                             '("autoload -Uz add-zsh-hook"
                               "add-zsh-hook precmd window_title")))))
         (cons 'documents (list (local-file "ghostscript.zsh")))
         (cons 'nix
               (list
                (plain-file "nix-profile.zsh"
                            ". ~/.nix-profile/etc/profile.d/nix.sh"))))))

(home-environment
  (packages
    (map specification->package (use-packages)))
  (services
   (list
    (service
     home-zsh-service-type
     (home-zsh-configuration
      (environment-variables (use-env))
      (zshrc (zshrc-files))))
    (dotfile-service 'git-dot-config "gitconfig" (git-config)))))
