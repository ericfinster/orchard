//
// orchard.js - Javascript Helper Routines
//
// Eric Finster
//

$(function() {

    window.orchard = {
	version : "0.1",
	panels : { },
	
	registerPanel : function(panelId, panelObj) {
	    console.log("Registering a panel with id: " + panelId);
	    orchard.panels[panelId] = panelObj;
	},

	getCell : function(panelId, cellId) {
	    //console.log("Getting cell " + cellId + " on panel " + panelId);

	    var theCell = orchard.panels[panelId].cells[cellId]

	    if (theCell == undefined) {
		console.log("Cell was undefined.")
	    }

	    return theCell
	},

	panelTest : function(panelId) {
	    var panelCells = orchard.panels[panelId].cells;

	    for (var key in panelCells) {
		console.log("Cell " + key + " has x-val " + panelCells[key].x());
		console.log("Cell " + key + " has y-val " + panelCells[key].y());
		console.log("Cell " + key + " has width " + panelCells[key].labelWidth());
		console.log("Cell " + key + " has height " + panelCells[key].labelHeight());
	    }

	    return (function () { if (1 > 2) { true } else { false } })();
	},

	renderPanel : function(panelId) {
	    console.log("Rendering panel: " + panelId);
	    // Go through the cells, look up the rectangle and the label and set their positions ...
	    var panel = orchard.panels[panelId];
	    var panelCells = panel.cells;
	    var panelEdges = panel.edges;
	    var baseCell  = panelCells[panel.baseCell];
	    
	    for (var key in panelCells) {
		$("#rect-" + key).attr( 
		    { 
			x : panelCells[key].x(),
			y : panelCells[key].y(),
			width : panelCells[key].width(),
			height : panelCells[key].height()
		    }
		);

		$("#label-" + key).attr(
		    {
			x : panelCells[key].labelX(),
			y : panelCells[key].labelY()
		    }
		);
	    };

	    for (var key in panelEdges) {
		var incomingX = panelEdges[key].incomingX();
		var incomingY = panelEdges[key].incomingY();
		var outgoingX = panelEdges[key].outgoingX();
		var outgoingY = panelEdges[key].outgoingY();

		var pathString = "M " +  incomingX + " " + incomingY + " "

		// This should be a global property somewhere ...
		var arcRadius = 4

		if (incomingX == outgoingX) {
		    pathString += ("V " + outgoingY)
		} else {
		    pathString += ("V " + (outgoingY - arcRadius))


		    if (incomingX > outgoingX) {
			pathString += (" A " + arcRadius + " " + arcRadius + " 0 0 1 " + (incomingX - arcRadius) + " " + outgoingY)
		    } else {
			pathString += (" A " + arcRadius + " "  + arcRadius + " 0 0 0 " + (incomingX + arcRadius) + " " + outgoingY)
		    }

		    pathString += " H " + outgoingX
		}
		
		$("#edge-" + key).attr(
		    {
			d : pathString
		    }
		);
	    }

	    var baseCellX = baseCell.x();
	    var baseCellY = baseCell.y();

	    var baseCellWidth = baseCell.width();
	    var baseCellHeight = baseCell.height();

	    var panelWidth = 0.0
	    var panelHeight = baseCellHeight + 20
	    var panelTranslateX = 0.0
	    var panelTranslateY = -baseCellY + 10

	    if (panel.leftEdge != null) {
		console.log("Has a left edge.")

		var leftEdgeX = panel.edges[panel.leftEdge].incomingX();
		var rightEdgeX = panel.edges[panel.rightEdge].incomingX();

		console.log("Left edge: " + leftEdgeX)
		console.log("Right edge: " + rightEdgeX)

		console.log("Base cell x: " + baseCellX)
		console.log("Base cell width: " + baseCellWidth)

		if (leftEdgeX < baseCellX) {
		    console.log("Edge underflow")
		}
		
		if (rightEdgeX > baseCellX + baseCellWidth) {
		    console.log("Edge overflow")
		}
		
		var panelLeftEdge = Math.min(leftEdgeX, baseCellX)
		var panelRightEdge = Math.max(rightEdgeX, baseCellX + baseCellWidth)

		panelWidth = panelRightEdge - panelLeftEdge + 20
		panelTranslateX = -panelLeftEdge + 10
	    } else {
		panelWidth = baseCellWidth + 20
		panelTranslateX = -baseCellX + 10
	    }

	    console.log("Translate: " + panelTranslateX)
	    console.log("Width: " + panelWidth)

	    $("#panel-" + panelId).attr(
		{
		    transform : "translate(" + panelTranslateX + ", " + panelTranslateY + ")"
		}
	    );

	    $("#panel-svg-" + panelId).attr(
	    	{
	    	    width : panelWidth,
	    	    height : panelHeight
	    	}
	    );
	},
	
	logAttr : function(cellId, attr) {
	    //console.log("Getting " + attr + " for cell " + cellId);
	}

    }

    $('.jcarousel').jcarousel({
        // Configuration goes here
    });

    $('.jcarousel-prev').jcarouselControl({
        target: '-=1'
    });

    $('.jcarousel-next').jcarouselControl({
        target: '+=1'
    });    

});

