type t<'a>;

let create:(int, 'a) => t<'a>;
let isEmpty:(t<'a>) => bool;
let read:(t<'a>) => option<'a>;
let pop:(t<'a>) => t<'a>;
let write:('a, t<'a>) => t<'a>;
