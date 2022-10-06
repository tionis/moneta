(declare-project
  :name "moneta"
  :description "basic money algorithms"
  :dependencies ["https://github.com/pyrmont/testament"
                 "https://github.com/janet-lang/spork"]
  :author "tionis.dev"
  :license "MIT"
  :url "https://tasadar.net/tionis/moneta"
  :repo "git+https://tasadar.net/tionis/moneta")

(declare-source
  :source ["moneta"])

#(declare-native
# :name "mynative"
# :source ["mynative.c" "mysupport.c"]
# :embedded ["extra-functions.janet"])

(declare-executable
  :name "moneta"
  :entry "moneta/cli.janet"
  :install true)
