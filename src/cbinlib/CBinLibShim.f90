! =============================================================================
! Phoenix Analysis &                                                     PADT
! Design Technologies 
! -----------------------------------------------------------------------------
! PURPOSE:  These routines function as an interoperability shim between
!           the BINLIB fortran functions and the C family of languages.
!           Each routine collects the arguments passed to it and forwards
!           them directly to the corresponding BINLIB routine.
#define MAXDOFPERNODE 256
#define MAXDOFLABELLEN ((MAXDOFPERNODE+1)*4+1)
#include "ansysdef.h"
! === CResRdBegin =============================================================
!     Shim function for ResRdBegin
!
    integer(c_int) function CResRdBegin (Fname, Title, JobName,              &
                                        Units, NumDOF, DOF, UserCode,        &
                                        MaxNode, NumNode, MaxElem, NumElem,  &
                                        MaxResultSet,NumResultSet)           &
                                        BIND(C,NAME="CResRdBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdBegin
    ! Fname:        The name (with extension) for the file 
    !               (Limit to 260 printable characters)
    character(len=1), dimension(261), intent(in) :: Fname
    ! Title:        The Title.  This is broken into strings of at most 80 
    !               characters.  Each subtitle in the title is separated by
    !               a newline character
    character(len=1), dimension(163), intent(out) :: Title
    ! JobName:      The jobname from file (Limit to 32 printable characters)
    character(len=1), dimension(33), intent(out) :: JobName
    ! Units:        Unit System
    !                = 0 - User Defined Units
    !                = 1 - SI
    !                = 2 - CSG
    !                = 3 - U.S. Customary, using feet
    !                = 4 - U.S. Customary, using inches
    !                = 5 - MKS
    !                = 6 - MPA
    !                = 7 - uMKS
    integer(c_int), intent(out) :: Units
    ! NumDOF:       The number of DOF per node  
    integer(c_int), intent(out) :: NumDOF
    ! DOF:          The DOFs per node  
    integer(c_int), dimension(MAXDOFPERNODE), intent(out) :: DOF
    ! UserCode:     Code for this application  
    integer(c_int), intent(out) :: UserCode
    ! MaxNode:      Maximum node number used  
    integer(c_int), intent(out) :: MaxNode
    ! NumNode:      Number of nodes attached to elements  
    integer(c_int), intent(out) :: NumNode
    ! MaxElem:      Maximum element number used  
    integer(c_int), intent(out) :: MaxElem
    ! NumElem:      Number of elements used  
    integer(c_int), intent(out) :: NumElem
    ! MaxResultSet: Maximum number of result sets (usually 1000)  
    integer(c_int), intent(out) :: MaxResultSet
    ! NumResultSet: Number of result sets on file  
    integer(c_int), intent(out) :: NumResultSet
    ! ResRdBegin:   0, successful  other, error in file open
    integer(c_int) :: ResRdBegin
    ! Internal variables for use with the call to the ANSYS
    ! ResRdBegin function
    character(261) :: FortFName
    integer ncFname
    integer Nunit, Lunit, ii, jj, idx
    character(80) :: FortTitle(2)
    character(32) :: FortJName
    ! Convert the input file name to a fortran character variable
    do ii=1, 261
        FortFName(ii:ii)=Fname(ii)
    end do
    ncFname = index(FortFName, C_NULL_CHAR) - 1
    ! Result file Unit number
    Nunit = 12
    ! Stdout Unit number
    Lunit = 6
    ! Call the base function
    CResRdBegin =                                                   &
        ResRdBegin(Nunit, Lunit, FortFName, ncFname, FortTitle(1),  &
                    FortJName, Units, NumDOF, DOF(1), UserCode,     &
                    MaxNode, NumNode, MaxElem, NumElem,             &
                    MaxResultSet, NumResultSet)
    ! Copy the jobname back over
    do ii = 1, len_trim(FortJName)
        JobName(ii) = FortJName(ii:ii)
    end do
    JobName(len_trim(FortJName)+1) = C_NULL_CHAR
    ! Copy the title back across
    idx=1
    do ii = 1, 2
        do jj = 1, len_trim(FortTitle(ii))
            Title(idx) = FortTitle(ii)(jj:jj)
            idx=idx+1
        end do
        Title(idx) = C_NEW_LINE
        idx=idx+1
    end do
    Title(idx) = C_NULL_CHAR
    end
! === CResRdEnd ===============================================================
!     Shim function for ResRdEnd
!    
    subroutine CResRdEnd() BIND(C,NAME="CResRdEnd")               
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdEnd
    call ResRdEnd()
    end 
! === CResRdGeomBegin =========================================================
!     Shim function for ResRdGeomBegin
!
    subroutine CResRdGeomBegin (MaxType, MaxReal, MaxCsys, nXYZ)   &
               BIND(C,NAME="CResRdGeomBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdGeomBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdGeomBegin
    ! MaxType:    The maximum element type
    integer(c_int), intent(out) :: MaxType
    ! MaxReal:    The maximum real constant set number
    integer(c_int), intent(out) :: MaxReal
    ! MaxCsys:    Maximum coordinate system number
    integer(c_int), intent(out) :: MaxCsys
    ! nXYZ:       The number of nodes with coordinates
    integer(c_int), intent(out) :: nXYZ
    ! Forward the call on to BINLIB
    call ResRdGeomBegin(MaxType, MaxReal, MaxCsys, nXYZ)
    end 

! === CResRdGeomEnd ===========================================================
!     Shim function for ResRdGeomEnd
!
    subroutine CResRdGeomEnd() BIND(C,NAME="CResRdGeomEnd")               
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdGeomEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdGeomEnd
    call ResRdGeomEnd()
    end 
    
! === CResRdSectMatBegin =====================================================
!     Shim function for ResRdSectMatBegin
!    
    subroutine CResRdSectMatBegin (MaxSect, MaxMat)                          &
        BIND(C,NAME="CResRdSectMatBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdSectMatBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdSectMatBegin
    ! MaxSect:      The maximum defined section number
    integer(c_int), intent(out) :: MaxSect
    ! MaxMat:       The maximum defined material set number
    integer(c_int), intent(out) :: MaxMat
    call ResRdSectMatBegin(MaxSect, MaxMat)
    end

! === CResRdSectMatEnd =======================================================
!     Shim function for ResRdSectMatEnd
!
    subroutine CResRdSectMatEnd() BIND(C,NAME="CResRdSectMatEnd")               
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdSectMatEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdSectMatEnd
    call ResRdSectMatEnd()
    end 
    
! === CResRdTypeBegin =========================================================
!     Shim function for ResRdTypeBegin
!
    subroutine CResRdTypeBegin(NumType) BIND(C,NAME="CResRdTypeBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdTypeBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdTypeBegin
    ! NumType:    The number of element types defined
    integer(c_int), intent(out) :: NumType
    call ResRdTypeBegin(NumType)
    end
! === CResRdType ==============================================================
!     Shim function for ResRdType
!   
    integer(c_int) function CResRdType (itype, ielc) BIND(C,NAME="CResRdType")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdTypeBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdType
    ! itype:     The element type number
    integer(c_int), intent(out) :: itype
    ! ielc:      An array of the element characteristics
    integer(c_int), dimension(IELCSZ), intent(out) :: ielc
    ! ResRdType: The number of words read
    integer(c_int) :: ResRdType
    CResRdType = ResRdType(itype, ielc)
    end
! === CResRdTypeEnd ===========================================================
!     Shim function for ResRdTypeEnd
!     
    subroutine CResRdTypeEnd() BIND(C,NAME="CResRdTypeEnd")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdTypeEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdTypeEnd
    call ResRdTypeEnd()
    end
! === CResRdRealBegin =========================================================
!     Shim function for ResRdRealBegin
!
    subroutine CResRdTypeBegin(NumReal, NumPerReal)                          &
               BIND(C,NAME="CResRdRealBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdRealBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdRealBegin
    ! NumReal:    The number of real constant sets
    integer(c_int), intent(out) :: NumReal
    ! NumPerReal: The maximum number of real constants for any set
    integer(c_int), intent(out) :: NumPerReal
    call ResRdRealBegin(NumReal, NumPerReal)
    end    
! === CResRdReal ==============================================================
!     Shim function for ResRdReal
!   
    integer(c_int) function CResRdReal (iReal, Rcon) BIND(C,NAME="CResRdReal")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdReal
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdType
    ! iReal:     The real constant set number
    integer(c_int), intent(in) :: iReal
    ! Rcon:      A double precision array of real constants
    real(c_double), dimension(*), intent(out) :: Rcon
    ! ResRdReal: The size of the real constant set
    integer(c_int) :: ResRdReal
    CResRdReal = ResRdReal(iReal, Rcon)
    end 
! === CResRdRealEnd ===========================================================
!     Shim function for ResRdRealEnd
! 
    subroutine CResRdRealEnd() BIND(C,NAME="CResRdRealEnd")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdRealEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdRealEnd
    call ResRdRealEnd()
    end
! === CResRdCsysBegin =========================================================
!     Shim function for ResRdCsysBegin
!   
    subroutine CResRdCsysBegin (NumCsys) BIND(C,NAME="CResRdCsysBegin") 
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdCsysBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdCsysBegin
    ! NumCsys:   Number of defined coordinate systems
    integer(c_int), intent(out) :: NumCsys
    call ResRdCsysBegin(NumCsys)
    end
! === CResRdCsys ==============================================================
!     Shim function for ResRdCsys
!     
    integer(c_int) function CResRdCsys (iCsys,Csys) BIND(C,NAME="CResRdCsys")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdCsys
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdCsys
    ! iCsys:      Coordinate system number
    integer(c_int), intent(in) :: iCsys
    ! Csys:       Coordinate system description (Len = ResRdCsys, Max = 24)
    real(c_double), dimension(24), intent(out) :: Csys
    ! ResRdCsys:  The number of data items in the description
    integer(c_int) :: ResRdCsys
    CResRdCsys = ResRdCsys(iCsys, Csys)
    end
! === CResRdCsysEnd ===========================================================
!     Shim function for ResRdCsysEnd
! 
    subroutine CResRdCsysEnd() BIND(C,NAME="CResRdCsysEnd")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdCsysEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdCsysEnd
    call ResRdCsysEnd()
    end    
! === CResRdSectBegin =========================================================
!     Shim function for ResRdSectBegin
!    
    subroutine CResRdSectBegin (NumSect, NumPerSect)                          &
        BIND(C,NAME="CResRdSectBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdSectBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdSectBegin
    ! NumSect:      Number of sections defined
    integer(c_int), intent(out) :: NumSect
    ! NumPerSect:   Maximum number of properties for any section
    integer(c_int), intent(out) :: NumPerSect
    call ResRdSectBegin(NumSect, NumPerSect)
    end
! === CResRdSect =============================================================
!     Shim function for ResRdSect
!  
    integer(c_int) function CResRdSect (iSect, SecData)                      &
        BIND(C, NAME="CResRdSect")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdSect
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdSect
    ! iSect:        Section Number
    integer(c_int), intent(in) :: iSect
    ! SecData:      The actual section data
    real(c_double), intent(out) :: SecData
    ! CResRdSect:   The number of section data items
    integer(c_int) :: ResRdSect
    CResRdSect = ResRdSect(iSect, SecData)
    end
! === CResRdSectEnd ==========================================================
!     Shim function for ResRdSectEnd
! 
    subroutine CResRdSectEnd() BIND(C,NAME="CResRdSectEnd")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdSectEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdSectEnd
    call ResRdSectEnd()
    end
! === CResRdMatBegin =========================================================
!     Shim function for ResRdMatBegin
!     
    subroutine CResRdMatBegin (NumMat, NumPerMat)                            &
        BIND(C,NAME="CResRdMatBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdMatBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdMatBegin
    ! NumMat:       The number of material sets defined
    integer(c_int), intent(out) :: NumMat
    ! NumPerMa:     Maximum number of material data in a set
    integer(c_int), intent(out) :: NumPerMat
    call ResRdMatBegin(NumMat, NumPerMat)
    end
! === CResRdMat ==============================================================
!     Shim function for ResRdMat 
!    
    integer(c_int) function CResRdMat (iMat, iProp, MatData)                 &
        BIND(C,NAME="CResRdMat")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdMat
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdMat
    ! iMat:         The material set number
    integer(c_int), intent(in) :: iMat
    ! iprop:        Property reference number
    !               See mpinqr for details
    integer(c_int), intent(in) :: iProp
    ! MatData:      An array of the material property data
    !               Array length is (CResRdMat)
    real(c_double), dimension(*), intent(out) :: MatData
    ! ResRdMat:     The size of the MatData array
    integer(c_int) :: ResRdMat
    CResRdMat = ResRdMat(iMat, iProp, MatData)
    end
! === CResRdMatEnd ===========================================================
!     Shim function for ResRdMatEnd 
!     
    subroutine CResRdMatEnd() BIND(C,NAME="CResRdMatEnd")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdMatEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdMatEnd
    call ResRdMatEnd()
    end
! === CResRdNodeBegin ========================================================
!     Shim function for ResRdNodeBegin
! 
    subroutine CResRdNodeBegin() BIND(C,NAME="CResRdNodeBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdNodeBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdNodeBegin
    call ResRdNodeBegin()
    end
! === CResRdNode =============================================================
!     Shim function for ResRdNode
!     
    integer(c_int) function CResRdNode (iNode, xyzang)                       &
        BIND(C,NAME="CResRdNode")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdNode
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdNode
    ! iNode:        The node index number (1 - nXYZnode)
    integer(c_int), intent(in) :: iNode
    ! xyzang:       The nodal coordinates and rotations
    real(c_double), dimension(6), intent(out) :: xyzang
    ! ResRdNode:    The actual node number
    integer(c_int) :: ResRdNode
    CResRdNode = ResRdNode(iNode, xyzang)
    end
! === CResRdNodeEnd ==========================================================
!     Shim function for ResRdNode
!    
    subroutine CResRdNodeEnd() BIND(C,NAME="CResRdNodeEnd")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdNodeEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdNodeEnd
    call ResRdNodeEnd()
    end
! === CResRdElemBegin ========================================================
!     Shim function for ResRdElemBegin
!     
    subroutine CResRdElemBegin() BIND(C,NAME="CResRdElemBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdElemBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdElemBegin
    call ResRdElemBegin()
    end
! === CResRdElem =============================================================
!     Shim function for ResRdElem
!     
    integer(c_int) function CResRdElem (iElem, nodes, ElemData)              &
        BIND(C,NAME="CResRdElem")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdElem
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdElem
    ! iElem:        The element number
    integer(c_int), intent(in) :: iElem
    ! nodes:        The nodes attached to the element
    integer(c_int), dimension(20), intent(out) :: nodes
    ! ElemData:     Element information
    !               mat    - material reference number
    !               type   - element type number
    !               real   - real constant reference number
    !               secnum - section number
    !               esys   - element coordinate system
    !               death  - death flag
    !                        = 0 - alive
    !                        = 1 - dead
    !               solidm - solid model reference
    !               shape  - coded shape key
    !               elnum  - element number
    !               pexcl  - P-Method exclude key
    integer(c_int), dimension(10), intent(out) :: ElemData
    ! ResRdElem:    Number of nodes
    integer(c_int) :: ResRdElem
    CResRdElem = ResRdElem(iElem, nodes, ElemData)
    end
! === CResRdElemEnd ==========================================================
!     Shim function for ResRdElemEnd
!   
    subroutine CResRdElemEnd() BIND(C,NAME="CResRdElemEnd")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdElemEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdElemEnd
    call ResRdElemEnd()
    end
! === CResRdSolBegin =========================================================
!     Shim function for ResRdSolBegin
!    
    integer(c_int) function CResRdSolBegin (key, lstep, substep, ncumit,     &
                                            kcmplx, time, Title, DofLab)     &
        BIND(C,NAME="CResRdSolBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdSolBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdSolBegin
    ! key            - 0, find by set number
    !                  1, find by lstep/substep
    !                  2, find by ncumit
    !                  3, find by time
    integer(c_int), intent(in) :: key
    ! lstep          - Load step number
    !                   if key=0, this is the set number
    integer(c_int), intent(inout) :: lstep
    ! substep        - Substep of this load step
    integer(c_int), intent(inout) :: substep
    ! ncumit         - Cumulative iteration number
    integer(c_int), intent(inout) :: ncumit
    ! kcmplx         - 0, Real solution   1, Imaginary solution
    integer(c_int), intent(in) :: kcmplx
    ! time           - Current solution time
    real(c_double), intent(inout) :: time
    ! Title          - Title and 4 subtitles
    character(len=1), dimension(405), intent(out) :: Title
    ! DofLab         - Labels for DOFs. Four characters each with each
    !                  label separated by a new line character
    character(len=1), dimension(MAXDOFLABELLEN), intent(out) :: DofLab
    ! ResRdSolBegin  - 0, requested solution set found
    !                  1, not found
    integer(c_int) :: ResRdSolBegin
    
    ! More fortran friendly arguments
    integer :: idx, ii, jj
    character(len=80) :: FortTitle(5)
    character(len=4) :: FortDOFLabels(MAXDOFPERNODE)
    CResRdSolBegin = ResRdSolBegin(key, lstep, substep, ncumit, kcmplx,      &
                                   time, FortTitle(1), FortDOFLabels(1))
    ! Copy the title and labels back across
    idx=1
    do ii = 1, 5
        do jj = 1, len_trim(FortTitle(ii))
            Title(idx) = FortTitle(ii)(jj:jj)
            idx=idx+1
        end do
        Title(idx) = C_NEW_LINE
        idx=idx+1
    end do
    Title(idx) = C_NULL_CHAR
    
    idx=1
    do ii = 1, MAXDOFPERNODE
        if (len_trim(FortDOFLabels(ii)) > 0) then
            do jj = 1, len_trim(FortDOFLabels(ii))
                DofLab(idx) = FortDOFLabels(ii)(jj:jj)
                idx=idx+1
            end do
            DofLab(idx) = C_NEW_LINE
            idx=idx+1
        end if
    end do
    DofLab(idx) = C_NULL_CHAR
    end
! === CResRdSolEnd ===========================================================
!     Shim function for ResRdSolEnd
!     
    subroutine CResRdSolEnd() BIND(C,NAME="CResRdSolEnd")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdSolEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdSolEnd
    call ResRdSolEnd()
    end
! === CResRdDispBegin =======================================================
!     Shim function for ResRdDispBegin
! 
    subroutine CResRdDispBegin() BIND(C,NAME="CResRdDispBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdDispBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdDispBegin
    call ResRdDispBegin()
    end
! === CResRdDisp ============================================================
!     Shim function for ResRdDisp
! 
    integer(c_int) function CResRdDisp (node, Disp) BIND(C,NAME="CResRdDisp")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdDisp
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdDisp
    ! node          - Node number
    integer(c_int), intent(in) :: node
    ! Disp          - Displacements (dp,ar(nDOF),out)
    real(c_double), dimension(MAXDOFPERNODE), intent(out) :: Disp
    ! ResRdDisp     - Number of displacements
    integer(c_int) :: ResRdDisp
    CResRdDisp = ResRdDisp(node, Disp)
    end
! === CResRdDispEnd =========================================================
!     Shim function for ResRdDispEnd
! 
    subroutine CResRdDispEnd() BIND(C,NAME="CResRdDispEnd")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdDispEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdDispEnd
    call ResRdDispEnd()
    end
! === CResRdRforBegin ======================================================
!     Shim function for ResRdRforBegin
!
    subroutine CResRdRforBegin (nRForce) BIND(C,NAME="CResRdRforBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdRforBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdRforBegin
    ! nRForce       - Number of reaction forces
    integer(c_int), intent(out) :: nRForce
    call ResRdRforBegin(nRForce)
    end
! === CResRdRfor ===========================================================
!     Shim function for ResRdRfor
!
    integer(c_int) function CResRdRfor (node,idof,value)                   &
        BIND(C,NAME="CResRdRfor")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdRfor
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdRfor
    ! node          - External node number
    integer(c_int), intent(in) :: node
    ! idof          - Internal DOF number
    integer(c_int), intent(in) :: idof
    ! value         - Value of reaction force
    real(c_double), intent(out) :: value
    ! ResRdRfor     - The number of results returned (0 or 1)
    integer(c_int) :: ResRdRfor
    CResRdRfor = ResRdRfor(node, idof, value)
    end
! === CResRdRforEnd ===========================================================
!     Shim function for ResRdRforEnd
!    
    subroutine CResRdRforEnd() BIND(C,NAME="CResRdRforEnd")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdRforEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdRforEnd
    call ResRdRforEnd()
    end
! === CResRdBCBegin ===========================================================
!     Shim function for ResRdBCBegin
!        
    subroutine CResRdBCBegin (BCHeader) BIND(C,NAME="CResRdBCBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdBCBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdBCBegin
    ! BCHeader      - Boundary Condition Header Structure
    integer(c_int), dimension(40), intent(in) :: BCHeader
    call ResRdBCBegin(BCHeader)
    end
! === CResRdBCEnd =============================================================
!     Shim function for ResRdBCEnd
!    
    subroutine CResRdBCEnd() BIND(C,NAME="CResRdBCEnd")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdBCEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdBCEnd
    call ResRdBCEnd()
    end
! === CResRdFixBegin ==========================================================
!     Shim function for ResRdFixBegin
! 
    subroutine CResRdFixBegin(BCHeader, nFixed) BIND(C,NAME="CResRdFixBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdFixBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdFixBegin
    ! BCHeader      - The boundary condition header info
    integer(c_int), dimension(40), intent(in) :: BCHeader
    ! nFixed        - The number of fixed supports
    integer(c_int), intent(out) :: nFixed
    call ResRdFixBegin(BCHeader, nFixed)
    end
! === CResRdFix ===============================================================
!     Shim function for ResRdFix
!
    integer(c_int) function CResRdFix (node, idof, value)                     &
        BIND(C,NAME="CResRdFix")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdFix
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdFix
    ! node          - External node number
    integer(c_int), intent(in) :: node
    ! idof          - Internal dof number
    integer(c_int), intent(in) :: idof
    ! value         - The fixed constraint value (Real, Imag, RealOld, ImagOld)
    real(c_double), dimension(4), intent(out) :: value
    ! ResRdFix      - The number of values read. (0 - 4)
    integer(c_int) :: ResRdFix
    CResRdFix = ResRdFix(node, idof, value)
    end
! === CResRdFixEnd ============================================================
!     Shim function for ResRdFixEnd
!    
    subroutine CResRdFixEnd() BIND(C,NAME="CResRdFixEnd")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdFixEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdFixEnd
    call ResRdFixEnd()
    end
! === CResRdForcBegin =========================================================
!     Shim function for ResRdForcBegin
!      
    subroutine CResRdForcBegin (BCHeader, nForces)                            &
        BIND(C,NAME="CResRdForcBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdForcBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdForcBegin
    ! BCHeader      - The boundary condition header info
    integer(c_int), dimension(40), intent(in) :: BCHeader
    ! nForces       - The number of forces
    integer(c_int), intent(out) :: nForces
    call ResRdForcBegin(BCHeader, nForces)
    end
! === CResRdForc ===============================================================
!     Shim function for ResRdForc
!    
    integer(c_int) function CResRdForc (node, idof, value)                     &
        BIND(C,NAME="CResRdForc")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdForc
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdForc
    ! node          - External node number
    integer(c_int), intent(in) :: node
    ! idof          - Internal dof number
    integer(c_int), intent(in) :: idof
    ! value         - The force value (Real, Imag, RealOld, ImagOld)
    real(c_double), dimension(4), intent(out) :: value
    ! ResRdForce    - The number of force values read. (0 - 4)
    integer(c_int) :: ResRdForc
    CResRdForc = ResRdForc(node, idof, value)
    end
! === CResRdForcEnd ============================================================
!     Shim function for ResRdForcEnd
! 
    subroutine CResRdForcEnd() BIND(C,NAME="CResRdForcEnd")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdForcEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdForcEnd
    call ResRdForcEnd()
    end
! === CResRdEresBegin ==========================================================
!     Shim function for ResRdEresBegin
!     
    subroutine CResRdEresBegin() BIND(C,NAME="CResRdEresBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdEresBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdEresBegin
    call ResRdEresBegin()
    end
! === CResRdEresEnd ============================================================
!     Shim function for ResRdEresEnd
!    
    subroutine CResRdEresEnd() BIND(C,NAME="CResRdEresEnd")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdEresEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdEresEnd
    call ResRdEresEnd()
    end
! === CResRdEstrBegin ==========================================================
!     Shim function for ResRdEresBegin
!     
    integer(c_int) function CResRdEstrBegin (iElem)                            &
        BIND(C,NAME="CResRdEstrBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdEstrBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdEstrBegin
    ! iElem          - Element Number
    integer(c_int), intent(in) :: iElem
    ! ResRdEstrBegin - 0 for no error, 1 for error
    integer(c_int) :: ResRdEstrBegin
    CResRdEstrBegin = ResRdEstrBegin(iElem)
    end
! === CResRdEstr ===============================================================
!     Shim function for ResRdEstr
!         
    integer(c_int) function CResRdEstr (iStr, Str) BIND(C,NAME="CResRdEstr")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdEstr
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdEstr
    ! iStr          - element record number (1-25)
    integer(c_int), intent(in) :: iStr
    ! Str           - element stress records
    real(c_double), dimension(*), intent(out) :: Str
    ! ResRdEstr     - Number of element values
    integer(c_int) :: ResRdEstr
    CResRdEstr = ResRdEstr(iStr, Str)
    end
! === CResRdEstrEnd ============================================================
!     Shim function for ResRdEstrEnd
!    
    subroutine CResRdEstrEnd() BIND(C,NAME="CResRdEstrEnd")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdEstrEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdEstrEnd
    call ResRdEstrEnd()
    end
! === CResRdNstrBegin =========================================================
!     Shim function for ResRdNstrBegin
!
    subroutine CResRdNstrBegin (kNodStr) BIND(C,NAME="CResRdNstrBegin")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdNstrBegin
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdNstrBegin
    ! kNodStr       - The number of nodal stress vectors
    integer(c_int), intent(out) :: kNodStr
    call ResRdNstrBegin(kNodStr)
    end
! === CResRdNstr ==============================================================
!     Shim function for ResRdNstr
!   
    integer(c_int) function CResRdNstr (node, Nstr) BIND(C,NAME="CResRdNstr")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdNstr
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdNstr
    ! node          - Node number
    integer(c_int), intent(in) :: node
    ! Nstr          - The nodal stresses in the form of dp(6,kNodstr)
    real(c_double), dimension(6, *), intent(out) :: Nstr
    ! ResRdNsrt     - kNodStr
    integer(c_int) :: ResRdNstr
    CResRdNstr = ResRdNstr(node, Nstr)    
    end
! === CResRdNstrEnd ===========================================================
!     Shim function for ResRdNstrEnd
!    
    subroutine CResRdNstrEnd() BIND(C,NAME="CResRdNstrEnd")
    !DEC$ ATTRIBUTES DLLEXPORT :: CResRdNstrEnd
    use, intrinsic :: ISO_C_BINDING
    implicit none
    external ResRdNstrEnd
    call ResRdNstrEnd()
    end
    