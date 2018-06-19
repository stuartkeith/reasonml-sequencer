# experiment!

https://reasonml.github.io/docs/en/newcomer-examples.html
Creating a parametrized type?


type universityStudent = {gpa: float};

type response('studentType) = {
  status: int,
  student: 'studentType
};

let result: response(universityStudent) = fetchDataFromServer();
