//
// orchard.js - Javascript Helper Routines
//
// Eric Finster
//

function returnLabelDimensions() {
    var dimensionMap = {}

    $('.orch-label').each(function(i, e) {
	dimensionMap[e.getAttribute("id")] = e.getBBox();
    });
	
    return dimensionMap
}
