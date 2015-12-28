;(require 'org-exp)
(require 'org2blog-autoloads)

(setq org2blog/wp-blog-alist
      '(("blogname"
	 :url "http://xxx.wordpress.com/xmlrpc.php"
	 :username "a_name"
	 :password "a_password"
	 :default-title ""
	 :default-categories ("Something")
	 :tags-as-categories nil
	 :show 'show)
	("CI"
	 :url "http://xxx.org/xmlrpc.php"
	 :username "another name"
	 :password "another_pass"
	 :default-title ""
	 :default-categories ("Uncategorized")
	 :tags-as-categories nil)
	))
