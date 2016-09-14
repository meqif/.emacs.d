;;; Compiled snippets and support files for `rust-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'rust-mode
                     '(("whilel" "while let ${1:pattern} = ${2:expression} {\n      $0\n}\n" "while let pattern = expression { ... }" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/whilel" nil nil)
                       ("while" "while ${1:expression} {\n      $0\n}" "while expression { ... }" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/while" nil nil)
                       ("uns" "unsafe {\n    $0\n}" "unsafe { ... }" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/unsafe" nil nil)
                       ("type" "type ${1:TypeName} = ${2:TypeName};" "type TypeName = TypeName;" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/type" nil nil)
                       ("trait" "trait ${1:TraitName}$2 {\n    $0\n}" "trait TraitName { ... }" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/trait" nil nil)
                       ("todo" "//   __________  ____  ____\n//  /_  __/ __ \\/ __ \\/ __ \\\n//   / / / / / / / / / / / /\n//  / / / /_/ / /_/ / /_/ /\n// /_/  \\____/_____/\\____/\n//\n// $0" "todo" nil nil
                        ((yas-indent-line 'fixed)
                         (yas-wrap-around-region 'nil))
                        "/Users/meqif/.emacs.d/snippets/rust-mode/todo" nil nil)
                       ("test" "#[test]\nfn ${1:test_$2}() {\n    $0\n}" "test" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/test" "direct-keybinding" nil)
                       ("struct" "struct ${1:StructName} {\n    ${2:field_name}: ${3:type},\n}\n$0" "struct" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/struct" "direct-keybinding" nil)
                       ("spawn" "thread::spawn(${1:move }|${2:argument}| {\n    $0\n})" "thread::spawn(|| { ... })" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/spawn" nil nil)
                       ("printv" "println!(\"${1:variable}: {:?}\", ${1:$(yas/substr yas-text \"[^ ]*\")});$0" "println!(\"variable: {}\", variable)" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/printv" "direct-keybinding" nil)
                       ("println" "println!(\"${1:{$2}}\", ${3:variable});" "println!(\"{}\", variable)" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/println" nil nil)
                       ("match" "match ${1:expression} {\n    $2 => $3,\n}$0" "match" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/match" nil nil)
                       ("main" "fn main() {\n    $0\n}" "main" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/main" "direct-keybinding" nil)
                       ("macro" "macro_rules! ${1:name} {\n     ($2) => ($3);\n}" "macro_rules! name { (..) => (..); }" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/macro" nil nil)
                       ("impl" "impl ${2:${1:Trait} for }${3:Type} {\n    $0\n}" "impl Trait for Type { ... }" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/impl" "direct-keybinding" nil)
                       ("ifl" "if let ${1:Some($2)} = ${3:something} {\n    $0\n}" "if let ... = ... { ... }" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/ifl" nil nil)
                       ("if" "if ${1:condition} {\n    $2\n}${3: else {\n    $4\n}}" "if condition { ... } else { ... }" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/if" nil nil)
                       ("from" "impl From<${1:SourceType}> for ${2:TargetType} {\n    fn from(${3:value}: $1) -> $2 {\n        $0\n    }\n}" "from" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/from" nil nil)
                       ("for" "for ${1:var} in ${2:iterable} {\n    $0\n}" "for var in iterable { ... }" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/for" nil nil)
                       ("fns" "fn ${1:name}(&$2self${4:, $3}) ${6:-> ${7:ReturnType} }{\n    ${unimplemented!()}\n}" "fn name(&self, ...) -> Type { ... }" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/meqif/.emacs.d/snippets/rust-mode/fns" "direct-keybinding" nil)
                       ("fn" "fn ${1:name}($2) ${4:-> ${3:ReturnType} }{\n    ${unimplemented!()}\n}" "fn name(...) -> Type { ... }" nil nil
                        ((yas-indent-line 'fixed))
                        "/Users/meqif/.emacs.d/snippets/rust-mode/fn" "direct-keybinding" nil)
                       ("ec" "extern crate ${1:name};$0" "extern crate" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/extern_crate" "direct-keybinding" nil)
                       ("enum" "enum ${1:Type} {\n     $0\n}" "enum Type { ... }" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/enum" nil nil)
                       ("display" "impl ${1:fmt::}Display for ${2:Type} {\n    fn fmt(&self, f: &mut $1Formatter) -> $1Result {\n        write!(f, \"$2({})\", self.$0)\n    }\n}" "display" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/display" nil nil)
                       ("derive" "#[derive(${1:Trait})]" "#[derive(Trait)]" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/derive" nil nil)
                       ("default" "impl Default for ${1:Type} {\n    fn default() -> $1 {\n        $1$0\n    }\n}" "default" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/default" nil nil)
                       ("debug" "debug!(\"$1\");$0" "debug" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/debug" "direct-keybinding" nil)
                       ("dbg" "debug!(\"${1:object}: {:?}\", ${1:$(yas/substr yas-text \"[^ ]*\")});" "debug!(description, object)" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/dbg" "direct-keybinding" nil)
                       ("cfg" "#[cfg(${1:attribute})]" "#[cfg(attribute)]" nil nil nil "/Users/meqif/.emacs.d/snippets/rust-mode/cfg" nil nil)))


;;; Do not edit! File generated at Mon Sep  5 19:09:27 2016
