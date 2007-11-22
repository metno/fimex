#include "FeltCDMReader.h"

namespace MetNoUtplukk
{

FeltCDMReader::FeltCDMReader(std::string filename, std::string configFilename)
: filename(filename), configFilename(configFilename), feltFile(filename)
{
	// TODO: enable use of configFilename for Felt_File

	// fill the CDM;
	std::vector<MetNoFelt::Felt_Array> fArrays(feltFile.listFeltArrays());
	for (std::vector<MetNoFelt::Felt_Array>::iterator it = fArrays.begin(); it != fArrays.end(); ++it) {
		if (it == fArrays.begin()) {
			// set the global data for this feltFile derived from first data
			// TODO: translate producer-id to something useful
		}
		std::string projStr = MetNoFelt::getProjString(it->getIndexHeader(), it->getDataHeader(), it->getExtraInformation()); // TODO Felt?.getProjString
		if (projectionVariables.find(projStr) == projectionVariables.end()) {
			std::string projName("Projection" + projectionVariables.size());
			// projection-variable without datatype and dimension
			CDMVariable projVar(projName, CDM_NAT, std::vector<CDMDimension>());
			cdm.addVariable(projVar);
			projectionVariables[projStr] = projName;
			std::vector<CDMAttribute> projAttr = projStringToAttributes(projStr); // TODO projStringToVariable
			for (std::vector<CDMAttribute>::iterator attrIt = projAttr.begin(); attrIt != projAttr.end(); ++attrIt) {
				cdm.addAttribute(projName, *attrIt);
			}
		}
		
	}
}

FeltCDMReader::~FeltCDMReader()
{
}

}
