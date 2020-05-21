(setq easy-hugo-basedir "~/source/blog/"
      easy-hugo-postdir "content/"
      easy-hugo-url "http://127.0.0.1:1313"
      easy-hugo-preview-url "http://127.0.0.1:1313"
      easy-hugo-previewtime "20"
      easy-hugo-server-flag "-D"
      )
(add-hook 'easy-hugo-mode-hook 'leo-blog/easy-hugo)
