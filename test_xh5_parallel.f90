program test_xh5_parallel
  use fc
  use xh5for
  use MPI
  use PENF, only: I4P, I8P, R4P, R8P, str
  implicit none
  
  type(xh5for_t)             :: xh5

  real(R4P), dimension(5)    :: X = (/0.0, 1.0, 0.0, 0.0, 1.0/)
  real(R4P), dimension(5)    :: Y = (/0.0, 0.0, 1.0, 0.0, 1.0/)
  real(R4P), dimension(5)    :: Z = (/0.0, 0.0, 0.0, 1.0, 1.0/)
  integer(I4P), dimension(8) :: topology = (/0, 1, 2, 3,&
                                               1, 2, 3, 4/)
  real(R4P),    dimension(15) :: vectorvelocity = (/0,0,0, &
                                                      1,0,0, &
                                                      2,0,0, &
                                                      3,0,0, &
                                                      4,0,0/)
  real,dimension(5) :: f=(/1.0,2.0,3.0,4.0,5.0/)
  real :: flux(5,8)
  
  integer                    :: rank = 0
  integer                    :: mpierr
  
  integer :: i
  do i=1,8
    flux(:,i)=f
  enddo

  call MPI_INIT(mpierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, rank, mpierr)
  
  X=X+rank; Y=Y+rank; Z=Z+rank
  vectorvelocity = vectorvelocity+rank
  
   call xh5%Open(FilePrefix='fish', GridType=XDMF_GRID_TYPE_UNSTRUCTURED, Strategy=XDMF_STRATEGY_CONTIGUOUS_HYPERSLAB, Action=XDMF_ACTION_WRITE)
   call xh5%SetGrid(NumberOfNodes=5, NumberOfElements=2,TopologyType=XDMF_TOPOLOGY_TYPE_TETRAHEDRON, GeometryType=XDMF_GEOMETRY_TYPE_X_Y_Z)
   call xh5%WriteTopology(Connectivities = topology)
   call xh5%WriteGeometry(X = X, Y = Y, Z = Z)
   call xh5%WriteAttribute(Name='Velocity', Type=XDMF_ATTRIBUTE_TYPE_VECTOR ,Center=XDMF_ATTRIBUTE_CENTER_NODE , Values=vectorvelocity)
   
   do i=1,8
     call xh5%WriteAttribute(Name='flux_g'//to_str(i), Type=XDMF_ATTRIBUTE_TYPE_SCALAR,&
                            & Center=XDMF_ATTRIBUTE_CENTER_NODE, Values=flux(:,i))
   enddo
   
   call xh5%Close()
   call xh5%Free()
   
   call MPI_FINALIZE(mpierr)
 
 end program test_xh5_parallel
