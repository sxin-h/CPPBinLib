//===-- CBinLib.h - Define the C Interface to an ANSYS RST file -*- C++ -*-===//
//
//                                P A D T, Inc
//
// This file is distributed under the MIT Open source license.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the C declarations for the procedural interface to
/// the ANSYS BINLIB routines used to access and read data off of an ANSYS
/// result file (.rst, .rth, et...)
///
//===----------------------------------------------------------------------===//
#ifndef ANSYS_CBINLIB_H
#define ANSYS_CBINLIB_H

#ifdef __cplusplus
extern "C" {
#endif
/// \brief Opens the result file and extracts header information
///
/// Note, this call must be matched with a subsequent CResRdEnd() to ensure
/// that the file is closed and internal structures are deallocated properly.
///
/// \param fileName full path to the result file to open. (max 260 characters)
/// \param [out] Title the simulation separated by newlines. (max162 characters)
/// \param [out] Jobname for the simulation. (max 32 characters)
/// \param [out] Units designator:
///              - 0 User defined units
///              - 1 SI
///              - 2 CSG
///              - 3 U.S. Customary, using feet
///              - 4 U.S. Customary, using inches
///              - 5 MKS
///              - 6 MPA
///              - 7 uMKS
/// \param [out] NumDOF is the number of DOF per node
/// \param [out] DOF is a list of the DOF ids active in the model
/// \param [out] UserCode is an internal code for this application
/// \param [out] MaxNode is the maximum node number defined.  (Nodes may not be 
///        numbered contiguousl.y)
/// \param [out] NumNode is the number of defined nodes on the result file.
/// \param [out] MaxElem is the maximum element number defined (Elements may not
///        be numbered contiguously.)
/// \param [out] NumElem is the number of defined elements.
/// \param [out] MaxResultSet is the maximum number of result sets (usually this
///        is 1000)
/// \param [out] NumResultSet is the actual number of result sets on file
/// \return 0 if successful, otherwise a non-zero value is 
/// returned. 
///  
extern int CResRdBegin(char* FileName, char* Title, char* JobName, int* Units,
                       int* NumDOF, int* DOFs, int* UserCode, int* MaxNode, 
                       int* NumNode, int* MaxElem, int* NumElem, 
                       int* MaxResultSet, int* NumResultSet);

/// \brief Closes the result file and releases resources 
///  Note, this function is used in conjunction with CResRdOpen
///  
extern void CResRdEnd();

/// \brief Initiate reading model geometry/preprocessing data off of the result 
/// file. 
/// Note, this function is matched with CResRdGeomEnd().   
///  
/// \param [out] MaxType is the maximum element type number on file 
/// \param [out] MaxReal is the maximum real constant number on file 
/// \param [out] MaxCsys is the maximum coordinate system number on file 
/// \param [out] nXYZ is the number of defined nodes with coordinate data on 
/// file 
/// 
extern void CResRdGeomBegin(int* MaxType, int* MaxReal, int* MaxCsys, 
                            int* nXYZ);
/// \brief Release resources after reading model geometry and preprocessing data 
/// off of the result file. 
///  
extern void CResRdGeomEnd();
extern void CResRdSectMatBegin(int* MaxSec, int* MaxMat);
extern void CResRdSectMatEnd();

/// \brief Initiate reading element type information 
/// Note, this function is matched with CResRdType() to read the type data and 
/// CResRdTypeEnd() to complete the read. 
/// For example: 
///  \code
///  int NumType = 0;
///  CResRdTypeBegin(&NumType);
///  for(int i = 1; i <= MaxElementType; i++)
///      int dataRead = CResRdType(&i, &typeData);
///  CResRdTypeEnd();
///  \endcode
///  
///  \param [out] NumElementType is the number of defined element types
/// on file. Note this may be less than the maximum element type number due to 
/// the fact that element types are not required to be consecutively numbered. 
///  
extern void CResRdTypeBegin(int* NumElementType);
/// \brief Read the element type data 
/// \param ElementType is the element type number for which to retrieve data 
/// \param [out] ElementCharacteristics is an array of element data.  Max length 
/// is IELCSZ. 
/// \return Size of data read off of the file. 
extern int CResRdType(int *ElementType, int* ElementCharacteristic);
/// \brief Release the resources associated with reading element type 
/// information. 
extern void CResRdTypeEnd();
/// \brief Initiate reading real constant data 
/// \param [out] NumReal is the number of real constant sets on file 
/// \param [out] NumPerReal is the maximum number of real constant data for any 
/// set defined on file. 
extern void CResRdRealBegin(int* NumReal, int* NumPerReal);
/// \brief Read real constant data off of the file 
/// \param RealNum the real constant set number to for which to retrieve data 
/// \param RealData an array of real constant data read off of disk 
/// \return Size of data read off of the file. 
extern int CResRdReal(int* RealNum, double* RealData);
/// \brief Release the resources associated with reading real constant data off 
/// of the file 
extern void CResRdRealEnd();

extern void CResRdCsysBegin(int* NumCsys);
extern int CResRdCsys(int* iCsys, double* CsysData);
extern void CResRdCsysEnd();
extern void CResRdSectBegin(int* NumSect, int* NumPerSect);
extern int CResRdSect(int* iSect, double* SecData);
extern void CResRdSectEnd();
extern void CResRdMatBegin(int* NumMat, int* NumPerMat);
extern int CResRdMat(int* iMat, int* iProp, double* MatData);
extern void CResRdMatEnd();
extern void CResRdNodeBegin();
extern int CResRdNode(int* iNode, double* coordsAndAngles);
extern void CResRdNodeEnd();
extern void CResRdElemBegin();
extern int CResRdElem(int* iElem, int* nodes, int* ElemData);
extern void CResRdElemEnd();
extern int CResRdSolBegin(int* key, int* lstep, int* substep, int* numit,
                          int* kcmplx, double* time, char* Title, 
                          char* DofLab);
extern void CResRdSolEnd();
extern void CResRdDispBegin();
extern int CResRdDisp(int* node, double* Disp);
extern void CResRdDispEnd();
extern void CResRdRforBegin(int* nRForce);
extern int CResRdRfor(int* node, int* idof, double* value);
extern void CResRdRforEnd();
extern void CResRdBCBegin(int* BCHeader);
extern void CResRdBCEnd();
extern void CResRdFixBegin(int* BCHeader, int* nFixed);
extern int CResRdFix(int* node, int* idof, double* value);
extern void CResRdFixEnd();
extern void CResRdForcBegin(int* BCHeader, int* nForces);
extern int CResRdForc(int* node, int* idof, double* value);
extern void CResRdForcEnd();
extern void CResRdEresBegin();
extern void CResRdEresEnd();
extern void CResRdEstrBegin(int iElem);
extern void CResRdEstr(int* iStr, double* Str);
extern void CResRdEstrEnd();
extern void CResRdNstrBegin(int* kNodStr);
extern int CResRdNstr(int* node, double* Nstr);
extern void CResRdNstrEnd();

#ifdef __cplusplus
}
#endif


#endif


