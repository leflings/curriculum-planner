module Types

type Period =
    | A
    | B
    | X

type Code =
    | E of int * Period
    | F of int * Period

type Course = { CourseNo: string; CourseName: string; Placement: Code; ECTS: float }
type ZCourse = { ZNo: int; ZCode: int; ZECTS: int }
