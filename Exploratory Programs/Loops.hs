students :: [String]
students = ["Alice", "Bob", "Tim", "Tom", "Austin"]

printStudents students = do
    if (null students)
        then print "Completed"
        else do
             print $ head students
             printStudents $ tail students

caseWisePrintStudents  students
    | null students = print "Completed' "
    | otherwise = do
         print $ head students
         caseWisePrintStudents $ tail students


main :: IO ()
main = do
    caseWisePrintStudents students 
    printStudents students
