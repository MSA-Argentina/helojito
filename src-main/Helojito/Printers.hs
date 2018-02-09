module Helojito.Printers  where

import           Data.List              (find)
import           Text.PrettyPrint
import qualified Text.PrettyPrint.Boxes as B
import           Text.PrettyPrint.Boxes (Box)
import           Web.Helojito
import           Helojito.Util
import           Data.Time.Calendar     (Day, fromGregorian)


pSimpleTasks :: TaskList -> Doc
pSimpleTasks (TaskList ts) = vcat $ map pSimpleTask ts

pWeekTasks :: [Day] -> [TaskList] -> Box
pWeekTasks ds xs = B.hsep 1 B.left colls
  where
    colls = map makeCol $ zip ds xs
    box s = B.text "\9474" B.<> B.alignHoriz B.left colWidth (B.text s)
    colWidth = 7
    tasky Task { taskId=TaskId id' } = show id'
    total = show . sum . map taskHours
    makeCol (d, (TaskList ts)) = B.vcat B.left $ box (toDayName d) :
                                                 box (toShortDate d) :
                                                 box (total ts ++ "hs") :
                                                 map (box . tasky) ts

pMonthTasks :: [Day] -> [TaskList] -> Box
pMonthTasks ds xs = B.text ("Total: " ++ (show $ full xs)) B.//
                    days_b B.//
                    month_b
  where
    days = [take 3 . toDayName $ fromGregorian 2018 1 (7 + x) | x <- [0..6]]
    days_b = B.hsep 3 B.left (map B.text days)
    month_b = B.alignHoriz B.right 41 w1 B.// B.vcat B.left ws
    (w1:ws) = weeks
    weeks = map (B.hsep 1 B.left) week_boxes
    week_boxes = map (map box) weeks_l
    weeks_l = chunkLeftWhen (isSunday . fst) $ zip ds xs
    total = sum . map taskHours
    full = sum . map (\(TaskList ts) -> total ts)
    box (d, TaskList ts) = B.text (toDayNumber d ++ replicate 3 '\9472') B.//
                           (B.text "\9474" B.<>
                            B.alignHoriz B.left 4 (B.text . show $ total ts))


pDayTasks :: Day -> TaskList -> Doc
pDayTasks d (TaskList ts) = vcat $ title : separator : hs : separator : map pSimpleTask ts
  where
    separator = hcat . replicate 10 . char $ '-'
    title = text (toDayName d) <+> (text $ show d)
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

pSimpleTaskTypes :: TaskTypeList -> Doc
pSimpleTaskTypes (TaskTypeList ts) = vcat $ map pSimpleTaskType ts

pSimpleTaskType :: TaskType -> Doc
pSimpleTaskType TaskType { taskTypeId=TaskTypeId id'
                         , taskTypeName=name } = int id' <+> char '-' <+> toDoc name
