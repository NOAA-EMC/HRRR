module module_types
  type att_info
     character(len=:), allocatable :: cdata, attname
     integer :: attnum=-1, atttype=-1, attlen=-1
     integer :: idata=-1
  end type att_info

  type att_list
     integer :: natts=-1
     type(att_info), allocatable :: atts(:)
  end type att_list

  type var_info
     character(len=:), allocatable :: varname
     integer :: varid=-1, vartype=-1, ndims=-1, datasize=-1
     integer, allocatable :: dimids(:)
     integer, allocatable :: dimsize(:)
     real(kind=4), allocatable :: real(:)
     integer(kind=4), allocatable :: int(:)
  end type var_info
end module module_types
