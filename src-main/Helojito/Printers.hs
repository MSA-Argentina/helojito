module Helojito.Printers  where

import           Data.List              (find)
import           Text.PrettyPrint
import qualified Text.PrettyPrint.Boxes as B
import           Text.PrettyPrint.Boxes (Box)
import           Web.Helojito
import           Helojito.Util          (toDoc, toDayName)
import           Data.Time.Calendar     (Day)


pSimpleTasks :: TaskList -> Doc
pSimpleTasks (TaskList ts) = vcat $ map pSimpleTask ts

pWeekTaks :: [Day] -> [TaskList] -> Box
pWeekTaks ds xs = B.hsep 1 B.left colls
  where
    colls = map makeCol $ zip ds xs
    makeCol (d, (TaskList ts)) = B.vcat B.left $ box (toDayName d) : box (total ts ++ "hs") : map (box . tasky) ts
    box s = B.text "|" B.<> B.alignHoriz B.left colWidth (B.text s)
    colWidth = 7
    tasky Task { taskId=TaskId id' } = show id'
    total = show . sum . map taskHours

pDayTaks :: Day -> TaskList -> Doc
pDayTaks d (TaskList ts) = vcat $ title : separator : hs : separator : map pSimpleTask ts
  where
    separator = hcat . replicate 10 . char $ '-'
    title = text $ toDayName d
    hs = text "Total" <> colon <+> total <> text "hs"
    total = float . sum . map taskHours $ ts

pSimpleTask :: Task -> Doc
pSimpleTask Task { taskId=TaskId id'
                 , taskHours=hours
                 , taskName=name } = int id' <+> char '-' <+> toDoc name <>
                                         colon <+> float hours <> text "hs"

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
