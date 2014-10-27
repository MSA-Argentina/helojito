module Helojito.Printers (
    pSimpleProject
  , pSimpleTask
  , pExtraTask
) where

import           Data.List         (find)
import           Data.Text         (Text, unpack)
import           Web.Helojito
import           Text.PrettyPrint


textt :: Text -> Doc
textt = text . unpack

pSimpleProject :: Project -> Doc
pSimpleProject Project { projectId=ProjectId id'
                       , projectName=name } = int id' <+> char '-' <+> textt name

pSimpleTask :: Task -> Doc
pSimpleTask Task { taskId=TaskId id'
                 , taskHours=hours
                 , taskName=name } = int id' <+> char '-' <+> textt name <+>
                                         char '-' <+> text "Hours Worked On:" <+> float hours

pExtraTask :: Task -> ProjectList -> Doc
pExtraTask Task { taskId=TaskId id'
                , taskHours=hours
                , taskProject=proj_id
                , taskDescription=desc
                , taskName=name }
                (ProjectList ps) = text "ID:" <+> int id' $$
                                   text "Name:" <+> textt name $$
                                   text "Project:" <+> proj_name $$
                                   text "Description:" <+> textt desc $$
                                   text "Hours Worked On:" <+> float hours
  where
    proj_name = case find (\p -> projectId p == proj_id) ps of
                    Nothing -> text $ show proj_id
                    Just p -> textt $ projectName p
