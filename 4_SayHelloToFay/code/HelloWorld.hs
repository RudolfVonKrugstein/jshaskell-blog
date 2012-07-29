module HelloWorld where

import Language.Fay.FFI
import Language.Fay.Prelude

main :: Fay ()
main = write "Hello, World!"

write :: Foreign a => a -> Fay ()
write = foreignFay "document.write" FayNone
