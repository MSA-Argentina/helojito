module Helojito.Printers (
    pSimpleTask
  , pExtraTask
) where

import           Data.Text         (Text, unpack)
import           Web.Helojito
import           Text.PrettyPrint


textt :: Text -> Doc
textt = text . unpack

pSimpleTask :: Task -> Doc
pSimpleTask Task { taskId=TaskId id'
                 , taskHours=hours
                 , taskName=name } = int id' <+> char '-' <+> textt name <+>
                                         char '-' <+> text "Hours Worked On:" <+> float hours

pExtraTask :: Task -> Doc
pExtraTask Task { taskId=TaskId id'
                , taskHours=hours
                , taskProject=proj
                , taskDescription=desc
                , taskName=name } = text "ID:" <+> int id' $$
                                    text "Name:" <+> textt name $$
                                    text "Project:" <+> int proj $$
                                    text "Description:" <+> textt desc $$
                                    text "Hours Worked On:" <+> float hours
