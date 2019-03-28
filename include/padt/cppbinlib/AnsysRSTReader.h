//===----- AnsysRSTReader.h - Reader for ANSYS Result Files -----*- C++ -*-===//
//
//                                P A D T, Inc
//
// This file is distributed under the MIT Open source license.
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains a declaration for the main reader class used to read
/// an ANSYS result file.  Iterators and data structures are defined for the
/// various entities that can be read off of a result file.
///
//===----------------------------------------------------------------------===//
#ifndef ANSYS_RST_READER_H
#define ANSYS_RST_READER_H

//#define BOOST_PARAMETER_MAX_ARITY 12

#include <string>
#include <map>
#include <vector>
#include <stack>
#include <memory>
#include <Eigen/Eigen>
#include <boost/iterator/iterator_facade.hpp>


namespace AnsysResultFile
{

enum ANSYS_DOF_IDS {
	UX = 1, UY = 2, UZ = 3, ROTX = 4, ROTY = 5, ROTZ = 6, AX = 7, AY = 8,
	AZ = 9, VX = 10, VY = 11, VZ = 12 /** 13 - 15 are spares **/, WARP = 16,
	CONC = 17, HDSP = 18, PRES = 19, TEMP = 20, VOLT = 21, MAG = 22, ENKE = 23, ENDS = 24,
	EMF = 25, CURR = 26, SP01 = 27, SP02 = 28, SP03 = 29, SP04 = 30, SP05 = 31, SP06 = 32,
	TBOT = 33, TE2 = 34, TE3 = 35, TE4 = 36, TE5 = 37, TE6 = 38, TE7 = 39, TE8 = 40,
	TE9 = 41, TE10 = 42, TE11 = 43, TE12 = 44, TE13 = 45, TE14 = 46, TE15 = 47, TE16 = 48,
	TE17 = 49, TE18 = 50, TE19 = 51, TE20 = 52, TE21 = 53, TE22 = 54, TE23 = 55, TE24 = 56,
	TE25 = 57, TE26 = 58, TE27 = 59, TE28 = 60, TE29 = 61, TE30 = 62, TE31 = 63, TTOP = 64,
};

enum SOLUTION_REQUEST_TYPE {
	NO_SOLUTION_REQUEST,
	SET_NUMBER,
	TIME
};
/// \brief is a container for nodal geometric data (position, orientation) 
/// from the result file.
struct NodalCoordinateData {
	int Number;
	Eigen::Vector3d Coordinates;
	Eigen::Vector3d Rotations;
};

struct ElementGeometryData {
	int Number;
	int MatID;
	int ElementTypeID;
	int RealConstantID;
	int SectionID;
	int CoordinateSystemID;
	bool DeathFlag;
	int SolidModelReferenceID;
	int ShapeCode;
	int PMethodExcludeKey;
	std::vector<int> Nodes;
};

struct SolutionSummaryData {
	int LoadStep;
	int SubStep;
	int CumulativeIteration;
	double Time;
	std::vector<std::string> DOFs;
};

struct NodalDOFSolutionData {
	int NodeNumber;
	std::map<ANSYS_DOF_IDS, double> Solutions;
};

struct DOFData {
	DOFData(ANSYS_DOF_IDS id, const std::string& l) :
		dofId(id), label(l)
	{}
	ANSYS_DOF_IDS dofId;
	std::string label;
};

struct SolutionRequest
{
	SolutionRequest() : requestType(NO_SOLUTION_REQUEST) {}
	std::vector<ANSYS_DOF_IDS> RequestedDOFs;
	SOLUTION_REQUEST_TYPE requestType;
	double solutionTime;
	int setNumber;
};

// Iterator forward declarations
class NodeIterator;
class ElementIterator;
class ElementTypeIterator;
class RealConstantSetIterator;
class CoordinateSystemIterator;
class ResultSetSummaryIterator;
class NodalDOFSolutionIterator;



class RSTReader
{
public:
	enum QUERY_TYPE {
		Nodes,
		Elements,
		ElementTypes,
		RealConstants,
		CoordinateSystems,
		ResultSetSummaries,
		NodalDOFSolution,
		NodalDerivedSolution,
		ElementDerivedSolution
	};
	typedef std::vector<DOFData> DOFTypeListT;
	typedef DOFTypeListT::const_iterator DOFTypeIterator;
public:
	RSTReader();
	virtual ~RSTReader();
	bool open(const std::string &FileName);
	bool isOpen() const;
	void close();
	bool select(QUERY_TYPE QueryType, SolutionRequest RequestType = SolutionRequest());
	int numberOfNodesDefined() const;
	int numberOfElementsDefined() const;
	NodeIterator nodes_begin() const;
	NodeIterator nodes_end() const;
	ElementIterator elements_begin() const;
	ElementIterator elements_end() const;
	ElementTypeIterator element_types_begin() const;
	ElementTypeIterator element_types_end() const;
	RealConstantSetIterator real_constants_begin() const;
	RealConstantSetIterator real_constants_end() const;
	ResultSetSummaryIterator result_set_summaries_begin() const;
	ResultSetSummaryIterator result_set_summaries_end() const;
	NodalDOFSolutionIterator nodal_dof_solution_begin() const;
	NodalDOFSolutionIterator nodal_dof_solution_end() const;
	DOFTypeIterator dof_types_begin() const;
	DOFTypeIterator dof_types_end() const;

	/// Generic class for ranges.
	template<
		typename CONTAINER_TYPE,
		typename ITER_TYPE,
		ITER_TYPE(CONTAINER_TYPE::*begin_fn)() const,
		ITER_TYPE(CONTAINER_TYPE::*end_fn)() const>
	class EntityRange {
	public:
		EntityRange(CONTAINER_TYPE &container) : container_(container) {}
		ITER_TYPE begin() const { return (container_.*begin_fn)(); }
		ITER_TYPE end() const { return (container_.*end_fn)(); }
	private:
		CONTAINER_TYPE &container_;
	};
	typedef EntityRange<
		const RSTReader,
		NodeIterator,
		&RSTReader::nodes_begin,
		&RSTReader::nodes_end> ConstNodesRange;

	typedef EntityRange<
		const RSTReader,
		ElementIterator,
		&RSTReader::elements_begin,
		&RSTReader::elements_end> ConstElementsRange;

	typedef EntityRange<
		const RSTReader,
		ResultSetSummaryIterator,
		&RSTReader::result_set_summaries_begin,
		&RSTReader::result_set_summaries_end> ConstResultSetSummaryRange;

	typedef EntityRange<
		const RSTReader,
		NodalDOFSolutionIterator,
		&RSTReader::nodal_dof_solution_begin,
		&RSTReader::nodal_dof_solution_end> ConstNodalDOFSolutionRange;

	typedef EntityRange<
		const RSTReader,
		DOFTypeIterator,
		&RSTReader::dof_types_begin,
		&RSTReader::dof_types_end> ConstDOFTypeRange;

	ConstNodesRange nodes() const {
		return ConstNodesRange(*this);
	}

	ConstElementsRange elements() const {
		return ConstElementsRange(*this);
	}

	ConstResultSetSummaryRange result_set_summaries() const {
		return ConstResultSetSummaryRange(*this);
	}

	ConstNodalDOFSolutionRange nodal_dof_solution() const {
		return ConstNodalDOFSolutionRange(*this);
	}

	ConstDOFTypeRange dof_types() const {
		return ConstDOFTypeRange(*this);
	}

private:
	bool selectNodes();
	bool selectElements();
	bool selectElementTypes();
	bool selectRealConstants();
	bool selectCoordinateSystems();
	bool selectSolutionSummaries();
	bool selectNodalDOFSolution(SolutionRequest requestType);
	
	/// \brief section level ids we use to denote the particular
	/// file section we are in so that we don't have to rely
	/// on RTTI type information
	enum ResultFileSectionLevel
	{
		UndefinedLevel=-1,
		BeginLevel,
		GeometryLevel,
		SectionAndMaterialLevel,
		SectionLevel,
		MaterialLevel,
		ElementTypeLevel,
		RealConstantLevel,
		CoordinateSystemLevel,
		NodeGeometryLevel,
		ElementGeometryLevel,
		SolutionLevel,
		NodalDOFSolutionLevel,
		ElementDerivedSolutionLevel,
		NodalDerivedSolutionLevel
	};
	ResultFileSectionLevel getLevel() const;
	ResultFileSectionLevel 
		reduceToLevelOrBegin(ResultFileSectionLevel desiredLevel);

	/// \brief an interface class for handling the movement between
	/// sections of the result file.  
	class ResultFileSection
	{
	public:
		virtual ~ResultFileSection() {}
		virtual ResultFileSectionLevel getLevel() const = 0;
	};

	class BeginSection : public ResultFileSection
	{
	public:
		virtual ~BeginSection();
		virtual ResultFileSectionLevel getLevel() const;
	};
	class GeometryDataSection : public ResultFileSection
	{
	public:
		GeometryDataSection() {}
		GeometryDataSection(int *MaxType, int *MaxReal,
			int *MaxCSYS, int *MaxNodesWithCoords);
		virtual ~GeometryDataSection();
		virtual ResultFileSectionLevel getLevel() const;
	};
	class NodalGeometricDataSection : public ResultFileSection
	{
	public:
		NodalGeometricDataSection(); 
		virtual ~NodalGeometricDataSection();
		virtual ResultFileSectionLevel getLevel() const;
	};
	class ElementGeometricDataSection : public ResultFileSection
	{
	public:
		ElementGeometricDataSection(); 
		virtual ~ElementGeometricDataSection();
		virtual ResultFileSectionLevel getLevel() const;
	};
	class SolutionDataSection : public ResultFileSection
	{
	public:
		SolutionDataSection() {}
		virtual ~SolutionDataSection();
		virtual ResultFileSectionLevel getLevel() const;
	};
	class NodalDOFSolutionSection : public ResultFileSection
	{
	public:
		NodalDOFSolutionSection();
		virtual ~NodalDOFSolutionSection();
		virtual ResultFileSectionLevel getLevel() const;
	};
private:

	typedef std::unique_ptr<ResultFileSection> SectionPtrT;
	std::stack<SectionPtrT> m_sectionStack;
	std::string m_FileName;
	std::string m_JobName;
	std::vector<std::string> m_Titles;
	std::vector<ANSYS_DOF_IDS> m_AvailableDOFs;
	std::vector<ANSYS_DOF_IDS> m_RequestedDOFs;
	DOFTypeListT m_AvailableDOFTypes;
	std::map<ANSYS_DOF_IDS, std::string> m_DOFToLabelMap;
	int m_Units;
	int m_MaxNodeNum;
	int m_NumNodes;
	int m_MaxElemNum;
	int m_NumElems;
	int m_MaxResultSet;
	int m_NumResultSet;
	int m_MaxElementTypeID;
	int m_MaxRealConstantSetID;
	int m_MaxCoordinateSystemID;
	int m_NumNodesWithCoordinates;
};

	
class NodeIterator :
	public boost::iterator_facade<
	NodeIterator,
	NodalCoordinateData,
	boost::single_pass_traversal_tag>
{
public:
	NodeIterator();
	NodeIterator(const NodeIterator &other);
private:
	NodeIterator(int index, bool read=false);
	friend class boost::iterator_core_access;
	friend class RSTReader;
	void increment();
	bool equal(NodeIterator const& other) const;
	NodalCoordinateData& dereference() const;
	void readData();
private:
	std::unique_ptr<NodalCoordinateData> pData;
	int currentIndex;
};


class ElementIterator :
	public boost::iterator_facade<
	ElementIterator,
	ElementGeometryData,
	boost::single_pass_traversal_tag>
{
public:
	ElementIterator();
	ElementIterator(const ElementIterator &other);
private:
	ElementIterator(int index, int MaxElementNumber, bool read = false);
	friend class boost::iterator_core_access;
	friend class RSTReader;
	void increment();
	bool equal(ElementIterator const& other) const;
	ElementGeometryData& dereference() const;
	void readData();
private:
	std::unique_ptr<ElementGeometryData> pData;
	int currentIndex;
	int MaxElemNumber;
};

class ResultSetSummaryIterator :
	public boost::iterator_facade<
	ResultSetSummaryIterator,
	SolutionSummaryData,
	boost::single_pass_traversal_tag>
{
public:
	ResultSetSummaryIterator();
	ResultSetSummaryIterator(const ResultSetSummaryIterator &other);
private:
	ResultSetSummaryIterator(int index, bool read = false);
	friend class boost::iterator_core_access;
	friend class RSTReader;
	void increment();
	bool equal(ResultSetSummaryIterator const& other) const;
	SolutionSummaryData& dereference() const;
	void readData();
private:
	std::unique_ptr<SolutionSummaryData> pData;
	int currentStep;
};

class NodalDOFSolutionIterator :
	public boost::iterator_facade<
	NodalDOFSolutionIterator,
	NodalDOFSolutionData,
	boost::single_pass_traversal_tag>
{
public:
	NodalDOFSolutionIterator();
	NodalDOFSolutionIterator(const NodalDOFSolutionIterator &other);
private:
	NodalDOFSolutionIterator(int index,
	                         int MaxNode,
		                     const std::vector<ANSYS_DOF_IDS> &allDOFs,
							 const std::vector<ANSYS_DOF_IDS> &requestedDOFs,
							 bool read = false);
	friend class boost::iterator_core_access;
	friend class RSTReader;
	void increment();
	bool equal(NodalDOFSolutionIterator const& other) const;
	NodalDOFSolutionData& dereference() const;
	void readData();
private:
	std::unique_ptr<NodalDOFSolutionData> pData;
	std::vector<ANSYS_DOF_IDS> AllDOFList;
	std::vector<ANSYS_DOF_IDS> RequestedDOFList;
	int currentIndex;
	int MaxNodeNum;
};
}

#endif


