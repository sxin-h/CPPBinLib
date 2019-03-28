#include "AnsysRSTReader.h"
#include "CBinLib.h"
#include <cassert>
#include <array>
#include <boost/algorithm/string.hpp>
#include <boost/dynamic_bitset.hpp>

namespace AnsysResultFile
{

RSTReader::RSTReader() :
m_Units(-1),
m_MaxNodeNum(-1),
m_NumNodes(-1),
m_MaxElemNum(-1),
m_NumElems(-1),
m_MaxResultSet(-1),
m_NumResultSet(-1),
m_MaxElementTypeID(-1),
m_MaxRealConstantSetID(-1),
m_MaxCoordinateSystemID(-1),
m_NumNodesWithCoordinates(-1)
{
	m_DOFToLabelMap[UX]   = "UX";	m_DOFToLabelMap[UY]   = "UY";
	m_DOFToLabelMap[UZ]   = "UZ";	m_DOFToLabelMap[ROTX] = "ROTX";
	m_DOFToLabelMap[ROTY] = "ROTY";	m_DOFToLabelMap[ROTZ] = "ROTZ";
	m_DOFToLabelMap[AX]   = "AX";	m_DOFToLabelMap[AY]   = "AY";
	m_DOFToLabelMap[AZ]   = "AZ";	m_DOFToLabelMap[VX]   = "VX";
	m_DOFToLabelMap[VY]   = "VY";	m_DOFToLabelMap[VZ]   = "VZ";
	m_DOFToLabelMap[WARP] = "WARP";	m_DOFToLabelMap[CONC] = "CONC";
	m_DOFToLabelMap[HDSP] = "HDSP";	m_DOFToLabelMap[PRES] = "PRES";
	m_DOFToLabelMap[TEMP] = "TEMP";	m_DOFToLabelMap[VOLT] = "VOLT";
	m_DOFToLabelMap[MAG]  = "MAG";	m_DOFToLabelMap[ENKE] = "ENKE";
	m_DOFToLabelMap[ENDS] = "ENDS";	m_DOFToLabelMap[EMF]  = "EMF";
	m_DOFToLabelMap[CURR] = "CURR";	m_DOFToLabelMap[SP01] = "SP01";
	m_DOFToLabelMap[SP02] = "SP02";	m_DOFToLabelMap[SP03] = "SP03";
	m_DOFToLabelMap[SP04] = "SP04";	m_DOFToLabelMap[SP05] = "SP05";
	m_DOFToLabelMap[SP06] = "SP06";	m_DOFToLabelMap[TBOT] = "TBOT";
	m_DOFToLabelMap[TE2]  = "TE2";	m_DOFToLabelMap[TE3]  = "TE3";
	m_DOFToLabelMap[TE4]  = "TE4";	m_DOFToLabelMap[TE5]  = "TE5";
	m_DOFToLabelMap[TE6]  = "TE6";	m_DOFToLabelMap[TE7]  = "TE7";
	m_DOFToLabelMap[TE8]  = "TE8";	m_DOFToLabelMap[TE9]  = "TE9";
	m_DOFToLabelMap[TE10] = "TE10";	m_DOFToLabelMap[TE11] = "TE11";
	m_DOFToLabelMap[TE12] = "TE12";	m_DOFToLabelMap[TE13] = "TE13";
	m_DOFToLabelMap[TE14] = "TE14";	m_DOFToLabelMap[TE15] = "TE15";
	m_DOFToLabelMap[TE16] = "TE16";	m_DOFToLabelMap[TE17] = "TE17";
	m_DOFToLabelMap[TE18] = "TE18";	m_DOFToLabelMap[TE19] = "TE19";
	m_DOFToLabelMap[TE20] = "TE20";	m_DOFToLabelMap[TE21] = "TE21";
	m_DOFToLabelMap[TE22] = "TE22";	m_DOFToLabelMap[TE23] = "TE23";
	m_DOFToLabelMap[TE24] = "TE24";	m_DOFToLabelMap[TE25] = "TE25";
	m_DOFToLabelMap[TE26] = "TE26";	m_DOFToLabelMap[TE27] = "TE27";
	m_DOFToLabelMap[TE28] = "TE28";	m_DOFToLabelMap[TE29] = "TE29";
	m_DOFToLabelMap[TE30] = "TE30";	m_DOFToLabelMap[TE31] = "TE31";
	m_DOFToLabelMap[TTOP] = "TTOP";
}

RSTReader::~RSTReader()
{
	close();
}


bool RSTReader::open(const std::string &fileName)
{
	if (isOpen()) {
		close();
	}
	const int TITLE_SIZE = 163;
	const int JOBNAME_SIZE = 33;
	const int MAXDOF = 256;
	// These character arrays will be parsed to form the 
	// actual member data
	char title[TITLE_SIZE];
	char jobName[JOBNAME_SIZE];
	// DOF data will be stored internally using a more descriptive
	// format.  See m_DOF.
	int numDOF = 0;
	int DOF[MAXDOF];
	std::fill_n(DOF, MAXDOF, 0);
	// Not sure what use this might have.  Just junk it.
	int UserCode = 0;
	int success =
		CResRdBegin(const_cast<char *>(fileName.c_str()),
		title, jobName,&m_Units, &numDOF, DOF, &UserCode,
		&m_MaxNodeNum, &m_NumNodes, &m_MaxElemNum, &m_NumElems,
		&m_MaxResultSet, &m_NumResultSet);
	if (success == 0) {
		// Fill the jobname and title fields
		m_JobName = std::string(jobName);
		boost::split(m_Titles, std::string(title), 
			boost::is_any_of("\n"), 
			boost::algorithm::token_compress_on);
		// Fill the available DOF list
		m_AvailableDOFs.clear();
		m_AvailableDOFTypes.clear();
		for (int i = 0; i < numDOF; ++i) {
			ANSYS_DOF_IDS id = static_cast<ANSYS_DOF_IDS>(DOF[i]);
			m_AvailableDOFs.push_back(id);
			m_AvailableDOFTypes.push_back(DOFData(id, m_DOFToLabelMap[id]));
		}
		// Populate the DOF data
		// Push the BeginLevel onto the stack
		m_sectionStack.push(std::make_unique<BeginSection>());
		return true; 
	}
	return false;
}

bool RSTReader::isOpen() const
{
	return !m_sectionStack.empty();
}

void RSTReader::close()
{
	// Unwind the stack, closing the various sections
	// of the result file as the destructors are called
	while (!m_sectionStack.empty()) {
		m_sectionStack.pop();
	}
}

RSTReader::DOFTypeIterator 
RSTReader::dof_types_begin() const
{
	return m_AvailableDOFTypes.begin();
}
RSTReader::DOFTypeIterator 
RSTReader::dof_types_end() const
{
	return m_AvailableDOFTypes.end();
}

int RSTReader::numberOfNodesDefined() const
{
	return m_NumNodes;
}
int RSTReader::numberOfElementsDefined() const
{
	return m_NumElems;
}

bool RSTReader::select(QUERY_TYPE QueryType, SolutionRequest RequestType)
{
	// Bail out early if no file is open
	if (!isOpen()) {
		return false;
	}
	switch (QueryType) {
	case Nodes:
		return selectNodes();
	case Elements:
		return selectElements();
	case ElementTypes:
		return selectElementTypes();
	case RealConstants:
		return selectRealConstants();
	case CoordinateSystems:
		return selectCoordinateSystems();
	case ResultSetSummaries:
		return selectSolutionSummaries();
	case NodalDOFSolution:
		return selectNodalDOFSolution(RequestType);
		break;
	case NodalDerivedSolution:
		break;
	case ElementDerivedSolution:
		break;
	}
	return false;
}

bool RSTReader::selectNodes()
{
	auto level = reduceToLevelOrBegin(GeometryLevel);
	if (level == BeginLevel) {
		// Transistion to the geometry level
		CResRdGeomBegin(&m_MaxElementTypeID, &m_MaxRealConstantSetID,
			&m_MaxCoordinateSystemID, &m_NumNodesWithCoordinates);
		m_sectionStack.push(std::make_unique<GeometryDataSection>());
		level = GeometryLevel;
	}
	if (level == GeometryLevel) {
		// Transistion to the nodal data level
		m_sectionStack.push(std::make_unique<NodalGeometricDataSection>());
	}
	return true;
}

// The iterator for nodal data
NodeIterator::NodeIterator() : 
currentIndex(-1), pData(std::make_unique<NodalCoordinateData>())
{}

NodeIterator::NodeIterator(const NodeIterator &other) :
currentIndex(other.currentIndex), pData(std::make_unique<NodalCoordinateData>())
{
	*pData = *other.pData;
}

NodeIterator::NodeIterator(int index, bool read) : 
currentIndex(index), pData(std::make_unique<NodalCoordinateData>())
{
	pData = std::make_unique<NodalCoordinateData>();
	if (read) {
		readData();
	}
}

bool NodeIterator::equal(NodeIterator const& other) const
{
	return currentIndex == other.currentIndex;
}

void NodeIterator::increment()
{
	currentIndex++;
	readData();
}

void NodeIterator::readData()
{
	std::array<double, 6> nodalData;
	int NodeNumber = CResRdNode(&currentIndex, nodalData.data());
	pData->Number = NodeNumber;
	std::copy(nodalData.begin(), nodalData.begin() + 3, pData->Coordinates.data());
	std::copy(nodalData.begin() + 3, nodalData.end(), pData->Rotations.data());
}

NodalCoordinateData& NodeIterator::dereference() const
{
	return *pData;
}

NodeIterator RSTReader::nodes_begin() const
{
	assert(m_sectionStack.top()->getLevel() == NodeGeometryLevel &&
		"The file level stack is not correct for reading nodal geometry! Call select(Nodes) first!");
	return NodeIterator(1, true);
}
NodeIterator RSTReader::nodes_end() const
{
	assert(m_sectionStack.top()->getLevel() == NodeGeometryLevel &&
		"The file level stack is not correct for reading nodal geometry! Call select(Nodes) first!");
	return NodeIterator(m_NumNodesWithCoordinates+1);
}



bool RSTReader::selectElements()
{
	// Work our way up to either the geometry level or begin level
	auto level = reduceToLevelOrBegin(GeometryLevel);
	// Work our way back down to the Element Data level
	if (level == BeginLevel) {
		// Record the geometry level transition on the section stack
		m_sectionStack.push(std::make_unique<GeometryDataSection>(
			&m_MaxElementTypeID, &m_MaxRealConstantSetID,
			&m_MaxCoordinateSystemID, &m_NumNodesWithCoordinates));
		// Make sure the next transition occurs
		level = getLevel();
	}
	if (level == GeometryLevel) {
		// Record the element level transition on the section stack
		m_sectionStack.push(std::make_unique<ElementGeometricDataSection>());
	}
	return true;
}

ElementIterator::ElementIterator() : 
currentIndex(-1), pData(std::make_unique<ElementGeometryData>())
{}

ElementIterator::ElementIterator(const ElementIterator &other) :
currentIndex(other.currentIndex), 
MaxElemNumber(other.MaxElemNumber), 
pData(std::make_unique<ElementGeometryData>())
{
	*pData = *other.pData;
}

ElementIterator::ElementIterator(int index, int MaxElementNumber, bool read) :
currentIndex(index),
MaxElemNumber(MaxElementNumber), 
pData(std::make_unique<ElementGeometryData>())
{
	if (read) {
		readData();
	}
}

bool ElementIterator::equal(ElementIterator const& other) const
{
	return currentIndex == other.currentIndex;
}

void ElementIterator::increment()
{
	readData();
}

void ElementIterator::readData()
{
	std::array<int, 20> nodes;
	std::array<int, 10> elemData;
	int numNodes = 0;
	do {
		++currentIndex;
		numNodes = CResRdElem(&currentIndex, nodes.data(), elemData.data());
	} while (numNodes == 0 && currentIndex < MaxElemNumber);
	
	pData->Nodes.clear();
	pData->Nodes.resize(numNodes);
	std::copy(nodes.begin(), nodes.begin() + numNodes, pData->Nodes.begin());
	pData->MatID = elemData[0];
	pData->ElementTypeID = elemData[1];
	pData->RealConstantID = elemData[2];
	pData->SectionID = elemData[3];
	pData->CoordinateSystemID = elemData[4];
	pData->DeathFlag = (elemData[5] == 1);
	pData->SolidModelReferenceID = elemData[6];
	pData->ShapeCode = elemData[7];
	pData->Number = elemData[8];
	pData->PMethodExcludeKey = elemData[9];
}

ElementGeometryData& ElementIterator::dereference() const
{
	return *pData;
}


ElementIterator RSTReader::elements_begin() const
{
	assert(getLevel() == ElementGeometryLevel &&
		"The file level stack is not correct for reading element geometry! Call select(Elements) first!");
	return ElementIterator(0, m_MaxElemNum, true);
}
ElementIterator RSTReader::elements_end() const
{
	assert(getLevel() == ElementGeometryLevel &&
		"The file level stack is not correct for reading element geometry! Call select(Elements) first!");
	return ElementIterator(m_MaxElemNum + 1, m_MaxElemNum);
}


bool RSTReader::selectElementTypes()
{

	return true;
}

bool RSTReader::selectRealConstants()
{

	return true;
}

bool RSTReader::selectCoordinateSystems()
{
	return true;
}


ResultSetSummaryIterator::ResultSetSummaryIterator() :
currentStep(-1), pData(std::make_unique<SolutionSummaryData>())
{}

ResultSetSummaryIterator::ResultSetSummaryIterator(int index, bool read) :
currentStep(index), pData(std::make_unique<SolutionSummaryData>())
{
	if (read) {
		readData();
	}
}

ResultSetSummaryIterator::ResultSetSummaryIterator(const ResultSetSummaryIterator &other) :
currentStep(other.currentStep),
pData(std::make_unique<SolutionSummaryData>())
{
	*pData = *other.pData;
}

bool ResultSetSummaryIterator::equal(ResultSetSummaryIterator const& other) const
{
	return (currentStep == other.currentStep);
}

void ResultSetSummaryIterator::increment()
{
	currentStep++;
	readData();
}

void ResultSetSummaryIterator::readData()
{
	char Titles[512];
	char DOFLabels[2048];
	int key, lstep, substep, ncumit, kcmplx;
	double time;
	kcmplx = 0;
	key = 0;
	lstep = currentStep;
	int success = 
		CResRdSolBegin(&key, &lstep, &substep, &ncumit, 
		               &kcmplx, &time, Titles, DOFLabels);
	CResRdSolEnd();
	pData->CumulativeIteration = ncumit;
	pData->LoadStep = lstep;
	pData->SubStep = substep;
	pData->Time = time;
	std::vector<std::string> Labels;
	boost::split(Labels, std::string(DOFLabels),
		boost::is_any_of("\n"),
		boost::algorithm::token_compress_on);
	pData->DOFs.clear();
	std::copy_if(Labels.begin(), Labels.end(), std::back_inserter(pData->DOFs),
		[](const std::string &val)->bool { return !val.empty(); });

}

SolutionSummaryData& ResultSetSummaryIterator::dereference() const
{
	return *pData;
}

bool RSTReader::selectSolutionSummaries()
{
	reduceToLevelOrBegin(BeginLevel);
	return true;
}

ResultSetSummaryIterator RSTReader::result_set_summaries_begin() const
{
	assert(getLevel() == BeginLevel &&
		"The file level stack is not correct for reading solution summaries! Call select(ResultSetSummaries) first!");
	return ResultSetSummaryIterator(1, true);
}

ResultSetSummaryIterator RSTReader::result_set_summaries_end() const
{
	assert(getLevel() == BeginLevel &&
		"The file level stack is not correct for reading solution summaries! Call select(ResultSetSummaries) first!");
	return ResultSetSummaryIterator(m_NumResultSet + 1);
}


NodalDOFSolutionIterator::NodalDOFSolutionIterator() :
currentIndex(-1), MaxNodeNum(-1), pData(std::make_unique<NodalDOFSolutionData>())
{}

NodalDOFSolutionIterator::NodalDOFSolutionIterator(const NodalDOFSolutionIterator &other) :
currentIndex(other.currentIndex),
MaxNodeNum(other.MaxNodeNum),
AllDOFList(other.AllDOFList),
RequestedDOFList(other.RequestedDOFList),
pData(std::make_unique<NodalDOFSolutionData>())
{
	*pData = *other.pData;
}


NodalDOFSolutionIterator::NodalDOFSolutionIterator(
	int index,
	int MaxNode,
	const std::vector<ANSYS_DOF_IDS> &allDOFs,
	const std::vector<ANSYS_DOF_IDS> &requestedDOFS,
	bool read) :
	currentIndex(index), 
	MaxNodeNum(MaxNode),
	pData(std::make_unique<NodalDOFSolutionData>()),
	AllDOFList(allDOFs),
	RequestedDOFList(requestedDOFS)
{
	if (read) {
		readData();
	}
}

void NodalDOFSolutionIterator::increment()
{
	readData();
}

bool NodalDOFSolutionIterator::equal(NodalDOFSolutionIterator const& other) const
{
	return (other.currentIndex == currentIndex);
}

NodalDOFSolutionData& NodalDOFSolutionIterator::dereference() const
{
	return *pData;
}

void NodalDOFSolutionIterator::readData()
{
	int resultCount;
	std::array<double, 256> displacements;
	pData->Solutions.clear();
	do {
		++currentIndex;
		resultCount = CResRdDisp(&currentIndex, displacements.data());
	} while (resultCount == 0 && currentIndex < MaxNodeNum);
	if (resultCount == 0) {
		pData->NodeNumber = -1;
		return;
	}
	pData->NodeNumber = currentIndex;
	// Create a bitset that flags which DOFs to extract from
	// the stream and return
	boost::dynamic_bitset<> requestedDOFBitset(AllDOFList.size());
	requestedDOFBitset.reset();
	for (const auto DOF : RequestedDOFList) {
		auto loc = std::find(AllDOFList.begin(), AllDOFList.end(), DOF);
		if (loc != AllDOFList.end()) {
			requestedDOFBitset.set(std::distance(AllDOFList.begin(), loc), true);
		}
	}
	for (size_t i = 0; i < requestedDOFBitset.size(); ++i) {
		if (requestedDOFBitset.test(i)) {
			pData->Solutions[AllDOFList[i]] = displacements[i];
		}
	}
}

bool RSTReader::selectNodalDOFSolution(SolutionRequest RequestType)
{
	char Titles[512];
	char DOFLabels[2048];
	int key, lstep, substep, ncumit, kcmplx;
	double time;
	auto level = reduceToLevelOrBegin(BeginLevel);
	kcmplx = 0;
	if (level == BeginLevel) {
		switch (RequestType.requestType)
		{
		case TIME:
			time = RequestType.solutionTime;
			key = 3;
			break;
		case SET_NUMBER:
			lstep = RequestType.setNumber;
			key = 0;
			break;
		default:
			return false;
		}
		int success =
			CResRdSolBegin(&key, &lstep, &substep, &ncumit,
			&kcmplx, &time, Titles, DOFLabels);
		if (success == 0) {
			m_sectionStack.push(std::make_unique<SolutionDataSection>());
			// Move to the displacement section
			m_sectionStack.push(std::make_unique<NodalDOFSolutionSection>());
			m_RequestedDOFs = RequestType.RequestedDOFs;
		}
		else {
			return false;
		}
	}
	return true;
}


NodalDOFSolutionIterator RSTReader::nodal_dof_solution_begin() const
{
	assert(getLevel() == NodalDOFSolutionLevel &&
		"The file level stack is not correct for reading nodal dof solution! Call select(NodalDOFSolution) first!");
	return NodalDOFSolutionIterator(0, m_MaxNodeNum, m_AvailableDOFs, m_RequestedDOFs, true);
}

NodalDOFSolutionIterator RSTReader::nodal_dof_solution_end() const
{
	assert(getLevel() == NodalDOFSolutionLevel &&
		"The file level stack is not correct for reading nodal dof solution! Call select(NodalDOFSolution) first!");
	return NodalDOFSolutionIterator(m_MaxNodeNum+1, m_MaxNodeNum, m_AvailableDOFs, m_RequestedDOFs, false);
}


///////////////////////////////////////////////////////////////////////////////


RSTReader::ResultFileSectionLevel RSTReader::getLevel() const
{
	if (m_sectionStack.empty()) {
		return UndefinedLevel;
	}
	return m_sectionStack.top()->getLevel();
}

RSTReader::ResultFileSectionLevel 
RSTReader::reduceToLevelOrBegin(ResultFileSectionLevel desiredLevel)
{
	// Repeatedly pop file section items off of the stack until we get
	// to the level we want, or we end up all the way back at the begin
	// level
	while (getLevel() != desiredLevel && 
		   getLevel() != BeginLevel   && 
		   !m_sectionStack.empty()) {
		m_sectionStack.pop();
	}
	// Let the caller know where we ended up.
	return getLevel();
}

RSTReader::BeginSection::~BeginSection()
{
	// Close the file
	CResRdEnd();
}
RSTReader::ResultFileSectionLevel
RSTReader::BeginSection::getLevel() const
{
	return BeginLevel;
}

RSTReader::GeometryDataSection::GeometryDataSection(
	int *MaxType, int *MaxReal,
	int *MaxCSYS, int *MaxNodesWithCoords)
{
	CResRdGeomBegin(MaxType, MaxReal, MaxCSYS, MaxNodesWithCoords);
}

RSTReader::GeometryDataSection::~GeometryDataSection()
{
	CResRdGeomEnd();
}

RSTReader::ResultFileSectionLevel
RSTReader::GeometryDataSection::getLevel() const
{
	return GeometryLevel;
}

RSTReader::NodalGeometricDataSection::NodalGeometricDataSection()
{
	CResRdNodeBegin();
}

RSTReader::NodalGeometricDataSection::~NodalGeometricDataSection()
{
	CResRdNodeEnd();
}

RSTReader::ResultFileSectionLevel
RSTReader::NodalGeometricDataSection::getLevel() const
{
	return NodeGeometryLevel;
}

RSTReader::ElementGeometricDataSection::ElementGeometricDataSection()
{
	CResRdElemBegin();
}

RSTReader::ElementGeometricDataSection::~ElementGeometricDataSection()
{
	CResRdElemEnd();
}

RSTReader::ResultFileSectionLevel
RSTReader::ElementGeometricDataSection::getLevel() const
{
	return ElementGeometryLevel;
}

RSTReader::SolutionDataSection::~SolutionDataSection()
{
	CResRdSolEnd();
}

RSTReader::ResultFileSectionLevel 
RSTReader::SolutionDataSection::getLevel() const
{
	return SolutionLevel;
}


RSTReader::NodalDOFSolutionSection::NodalDOFSolutionSection()
{
	CResRdDispBegin();
}

RSTReader::NodalDOFSolutionSection::~NodalDOFSolutionSection()
{
	CResRdDispEnd();
}

RSTReader::ResultFileSectionLevel
RSTReader::NodalDOFSolutionSection::getLevel() const
{
	return NodalDOFSolutionLevel;
}


} // End namespace for AnsysResultFile

