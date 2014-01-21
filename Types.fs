module Types

type Subcode =
    | A
    | B
    | X

type Code =
    | E of int * Subcode
    | F of int * Subcode
    | Jan
    | Jun

type Course =
    { CourseNo: string;
     CourseName: string;
     Code: Code;
     ECTS: float;
     Prereqs: string list }

type ZCourse = { ZNo: int;
                ZCode: int;
                ZECTS: int;
                ZMandatory: bool } 

type Period =
    | Spring of string * (float * float)
    | Fall of string * (float * float)
    | January of string * (float * float)
    | June of string * (float * float)

type Semester =
    | Semester of Period * (Code list * Code list) * Set<Course> * string list

type MasterStudy =
    | MasterStudy of Semester list