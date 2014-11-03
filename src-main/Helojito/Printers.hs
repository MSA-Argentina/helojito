module Helojito.Printers  where

import           Data.List       (find)
import           Text.PrettyPrint
import           Web.Helojito
import           Helojito.Util   (toDoc)


pSimpleTasks :: TaskList -> Doc
pSimpleTasks (TaskList ts) = vcat $ map pSimpleTask ts

pSimpleTask :: Task -> Doc
pSimpleTask Task { taskId=TaskId id'
                 , taskHours=hours
                 , taskDate=date
                 , taskName=name } = int id' <+> char '-' <+> toDoc name <>
                                         char ':' <+> float hours <> text "hs -" <+> toDoc date

pExtraTask :: (Task, ProjectList) -> Doc
pExtraTask (Task { taskId=TaskId id'
                 , taskHours=hours
                 , taskProject=proj_id
                 , taskDescription=desc
                 , taskDate=date
                 , taskName=name }
           , (ProjectList ps)) = text "ID:" <+> int id' $$
                                   text "Name:" <+> toDoc name $$
                                   text "Project:" <+> proj_name $$
                                   text "Description:" <+> toDoc desc $$
                                   text "Date:" <+> toDoc date $$
                                   text "Hours Worked On:" <+> float hours
  where
    proj_name = case find (\p -> projectId p == proj_id) ps of
                        Nothing -> text "Project Not Found"
                        Just p -> toDoc $ projectName p

pSimpleProjects :: ProjectList -> Doc
pSimpleProjects (ProjectList ps) = vcat $ map pSimpleProject ps

pSimpleProject :: Project -> Doc
pSimpleProject Project { projectId=ProjectId id'
                       , projectName=name } = int id' <+> char '-' <+> toDoc name

pSimpleResolutions :: ResolutionList -> Doc
pSimpleResolutions (ResolutionList ps) = vcat $ map pSimpleResolution ps

pSimpleResolution :: Resolution -> Doc
pSimpleResolution Resolution { resId=ResolutionId id'
                             , resName=name } = int id' <+> char '-' <+> toDoc name

pSimpleTaskTypes :: TaskTypeList -> Doc
pSimpleTaskTypes (TaskTypeList ts) = vcat $ map pSimpleTaskType ts

pSimpleTaskType :: TaskType -> Doc
pSimpleTaskType TaskType { taskTypeId=TaskTypeId id'
                         , taskTypeName=name } = int id' <+> char '-' <+> toDoc name
