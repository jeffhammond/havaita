subroutine init(sh)
class(*) :: sh
select type(sh)
type is (integer)
  print*,'integer' ! integer specific code
type is (real)
  print*,'real' ! real specific code
type is (complex)
  print*,'complex' ! complex specific code
end select
end subroutine
