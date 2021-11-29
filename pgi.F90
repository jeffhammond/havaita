subroutine init(sh)
class(*) :: sh
select type(sh)
class is (shape)
  :   ! shape specific code
type is (integer)
  : ! integer specific code
type is (real)
  : ! real specific code
type is (complex)
  : ! complex specific code
end select
end subroutine
