//
// orchard.js - helper routines (and testing)
//

function returnLabelDimensions() {
    var dimensionMap = {}

    $('.orch-label').each(function(i, e) {
	dimensionMap[e.getAttribute("id")] = e.getBBox();
    });
	
    return dimensionMap
}

