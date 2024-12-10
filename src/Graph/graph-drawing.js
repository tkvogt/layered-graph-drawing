function insertStyleSheetRule(ruleText)
{
    let sheets = document.styleSheets;

    if(sheets.length == 0)
    {
        let style = document.createElement("style");
        style.appendChild(document.createTextNode(""));
        document.head.appendChild(style);
    }

    let sheet = sheets[sheets.length - 1];
    sheet.insertRule(ruleText, sheet.rules ? sheet.rules.length : sheet.cssRules.length);
}

var outerLeft = 0;
var animationActive = false;
var prevMiddle = 0, curMiddle = 0, prev, current, lastPrevAnim = null;
var newWidth = 0;
var activeTable;
var state = "";

function prevCur() {
  var fcol = document.getElementById("focusColumn");
  prev =    fcol.getAttribute("prev");
  current = fcol.getAttribute("current");
  state = fcol.getAttribute("positionx");

  var innerGraph = document.getElementById("inner-graph");

  if (prev.startsWith("Just")){
    var pr = prev.substr(5) + "cell";
    var prNode = document.getElementById(pr);
    if (prNode != null && innerGraph != null){
      prevMiddle = (prNode.getBoundingClientRect().left +
                    prNode.getBoundingClientRect().right) / 2
                   - innerGraph.getBoundingClientRect().left;
    }
  }

  if (current.startsWith("Just")) {
    var cur = current.substr(5) + "cell";
    var curNode = document.getElementById(cur);
    if (curNode != null){
      curMiddle = (curNode.getBoundingClientRect().left +
                   curNode.getBoundingClientRect().right) / 2 
                   - innerGraph.getBoundingClientRect().left;
    }
  }
}


function listener(event) {
  switch(event.type) {
    case "animationend":
      animationActive = false;
      if (document.getElementById("focusColumn") != null){ prevCur(); }
      break;
  }
}

function isUpper2BorderCell(divElement) {
    if (!(divElement instanceof HTMLDivElement)) {
        return false;
    }
    const style = window.getComputedStyle(divElement);
    return style.borderTopWidth !== '0px' && style.borderTopStyle !== 'none';
}

// Tests if the attribute border-top exists and is greater than 0px;
function isUpperBorderCell(divElement) {
    if (!(divElement instanceof HTMLDivElement)) {
        return false;
    }
    //console.log("divElement.style.borderTop " + divElement.style.borderTop);
    var str = divElement.style.borderTop;
    if (str.length === 0) { return false; }
    const firstChar = str[0];
    return !isNaN(firstChar) && firstChar !== '0';
}

// Tests if the attribute border-bottom exists and is greater than 0px;
function isLowerBorderCell(divElement) {
    if (!(divElement instanceof HTMLDivElement)) {
        return false;
    }
    //console.log("divElement.style.borderBottom " + divElement.style.borderBottom);
    var str = divElement.style.borderBottom;
    if (str.length === 0) { return false; }
    const firstChar = str[0];
    return !isNaN(firstChar) && firstChar !== '0';
}

// At first a html table was used to place cells,
// but it is visually better to set the cell height individually,
// for example when a node is much bigger than the the others
// Imagine this algorithms a little bit like a tetris game with blocks that have individual heights
function setHeightsOfColumnCells() {
  //console.log("setHeightsOfColumnCells ");
  for(var tableIter=0; document.getElementById("funcTable"+tableIter) != null; tableIter++)
  {
    var table = document.getElementById("funcTable"+tableIter);
    var cols = table.children;

    let tableArray = [];
    let tableArrayHeights = [];
    let cs = document.querySelectorAll('.functionTable .col');
    //console.log("cs" + cs);
    cs.forEach(col => {
      let colArray = [];
      let colArrayHeights = [];
      let cells = col.querySelectorAll('.nodeCell');
      cells.forEach(cell => {
        colArray.push(cell.id);
        colArrayHeights.push(document.getElementById(cell.id).offsetHeight);
      });
  
      tableArray.push(colArray);
      tableArrayHeights.push(colArrayHeights);
    });

    //showTable("start", tableArray,tableArrayHeights);

    var oneLayers = cols[0].children;
    //console.log("oneLayers " + oneLayers.length);

    for(var ol=0; ol<oneLayers.length; ol++)
    {
      //console.log("layer " + ol);
      var blocks = oneLayers[ol].children;
      for(var b=0; b < blocks.length; b++)
      {
        var nodes = blocks [b].children;
        var maxYOfBlock = 0;
        var finalRowMaxYOfBlock = 0;
        var finalRowValue = 0;
        let sumValues = []
        for(var n=0; n < nodes.length; n++) // go through all nodes of a block (See the DOM under <div class="functionTable"><div class="yblocks">)
        {
          var x = nodes[n].getAttribute('x');
          var nr = nodes[n].getAttribute('nr');
          var y = ol+1;
          //console.log("x " + x + " y " + y + " nr" + nr);
          sumValues[n] = 0;
          for (var ycount = y-1; ycount >= 0; ycount--)
          {
            let value = tableArray[x]?.[ycount];
             if(value !== 'undefined'){
              if (document.getElementById(value) !== null){
                var isUpper = isUpperBorderCell(document.getElementById(value));
                var isLower = isLowerBorderCell(document.getElementById(value));
                //console.log("value " + value + "  isUpper " + isUpper + "  isLower " + isLower + tableArrayHeights[x]?.[ycount]);
                sumValues [n] += tableArrayHeights[x]?.[ycount];
              }
            }
          }
          //console.log("sumValues [n] " + sumValues [n]);
          let nextValue = tableArray[x]?.[y];
          if(nextValue !== 'undefined'){
              if (document.getElementById(nextValue) !== null){
                var isUpper = isUpperBorderCell(document.getElementById(nextValue));
                var isLower = isLowerBorderCell(document.getElementById(nextValue));
                //console.log("value " + nextValue + "  isUpper " + isUpper + "  isLower " + isLower + tableArrayHeights[x]?.[y]/2);
                if(!isUpper) { 
                  if (isLower) { sumValues [n] += tableArrayHeights[x]?.[y]; }
                  else { sumValues [n] += tableArrayHeights[x]?.[y]/2; }
                }
              }
          }
          if (sumValues [n] > maxYOfBlock) {
            maxYOfBlock = sumValues [n];
          }

          let finalRowValue = tableArray[x]?.[y];
          if(finalRowValue !== 'undefined'){
            let cell = document.getElementById(finalRowValue);
            //console.log("x " + x + " y " + y + " " + nr + " " + value);
            if (cell !== null){
              if (cell.offsetHeight > finalRowMaxYOfBlock) {
                finalRowMaxYOfBlock = cell.offsetHeight;
              }
            }
          }
        }

        for(var n=0; n < nodes.length; n++)
        {
          var x = nodes[n].getAttribute('x');
          var nr = nodes[n].getAttribute('nr');
          var y = ol+1;
          let value = tableArray[x]?.[y-1];
          //console.log("x " + x + " y " + y + " value " + value);
          if(value !== 'undefined'){
            if (document.getElementById(value) !== null){

              // This is a weird hack, to count the height of sub divs, because for an unknown reason the div has a smaller offsetHwight than its sub elements
              //let totalHeight = 0;
              let cs = document.getElementById(value).firstChild.children;
              let totalHeight = 0;
              for (let i = 0; i < cs.length; i++) {
                totalHeight += cs[i].offsetHeight;
              }

              //console.log(" firstChild " + value + " " + document.getElementById(value).firstChild.offsetHeight + " offsetHeight " + document.getElementById(value).offsetHeight + " totalHeight " + totalHeight);
              //console.log("x " + x + " y " + y + " " + nr + " value " + value + " offsetHeight " + document.getElementById(value).offsetHeight);
              let offsetH = document.getElementById(value).offsetHeight;
              if (totalHeight > offsetH) {
                tableArrayHeights[x][y-1] = totalHeight + (maxYOfBlock - sumValues [n]);
              } else {
                tableArrayHeights[x][y-1] = offsetH + (maxYOfBlock - sumValues [n]);
              }
            }
          }

          let finalRowValue = tableArray[x]?.[y];
          if(finalRowValue !== 'undefined'){
            let cell = document.getElementById(finalRowValue);
            if (cell !== null){
              //document.getElementById(finalRowValue).style.height = finalRowMaxYOfBlock + "px";
            }
          }
        }
        //showTable("layer" + ol, tableArray, tableArrayHeights);
      }
    }
    //showTable("adjustCellHeights", tableArray, tableArrayHeights);
    adjustCellHeights(tableArray, tableArrayHeights);
  }
}

function adjustCellHeights(tableArray, tableArrayHeights)
{
  var rowlen = tableArray.length;
  var collen = tableArray[0].length;
  for (let y = 0; y < collen; y++) {
    for (let x = 0; x < rowlen; x++) {
      let value = tableArray[x]?.[y];
      let heights = tableArrayHeights[x]?.[y];
      if(value !== 'undefined' && heights !== 'undefined'){
        if (document.getElementById(value) !== null){
          document.getElementById(value).style.height = tableArrayHeights[x]?.[y] + "px";
        }
      }
    }
  }
}

function showTable(startString, tableArray, tableArrayHeights)
{ var rowlen = tableArray.length;
  var collen = tableArray[0].length;
  //    console.log(`rowlen ` + rowlen + `collen ` + collen);
  console.log(startString);
  for (let y = 0; y < collen; y++) {
    for (let x = 0; x < rowlen; x++) {
        console.log(`x ${x}, y ${y}: ${tableArray[x][y]} tableArrayHeights ${tableArrayHeights[x]?.[y]}`); //.textContent
    }
  }
}

function moveTables() {

  if( document.getElementById("activeTable") != null){
    activeTable = document.getElementById("activeTable");
  }
  var nr = activeTable.getAttribute("nr");

  // console.log("moveTables " + nr + activeTable);

  // placing search box and arrows by cell widths of bottom table cells
  var searchBoxRow = document.getElementById("activeSearchBox");
  var searchArrowsRow = document.getElementById("activeSearchArrows");
  var headRow = document.getElementById("funcTable"+nr);

  if(headRow != null)
  {
    if(searchBoxRow != null) {
      var searchBoxCells = searchBoxRow.children;
      var searchArrowsCells = searchArrowsRow.children;
      var headRowCells = headRow.firstChild.children;

      for(var col=0; col<headRowCells.length; col++)
      {
        var cell = headRowCells[col];
        var cellWidth;
        if (cell != null){
          cellWidth = cell.offsetWidth;
          if (cellWidth < 50 && cellWidth > 20) { cellWidth = 70; }
          //console.log("cellWidth " + cellWidth);
          if(searchBoxCells[col] != null)
          {  searchBoxCells[col].firstChild.style.width = cellWidth + "px"; }
          if(searchArrowsCells[col] != null && searchArrowsCells[col].firstChild != null)
          {  searchArrowsCells[col].firstChild.style.width = cellWidth + "px"; }
        }
      }
    }
  }

  var lrdiff = 0;
  if(headRowCells != null && state == "middle"){
    var cell0 = headRowCells[0];
    var cell1 = headRowCells[4];
    if (cell0 != null && cell1 != null){
      lrdiff = cell1.offsetWidth - cell0.offsetWidth;
    }
  }

  if (animationActive == false && document.getElementById("focusColumn") != null)
  {
    var w = window,
        d = document,
        e = d.documentElement,
        g = d.getElementsByTagName("body")[0],
        x = w.innerWidth || e.clientWidth || g.clientWidth;

    for(var tableIter=0; document.getElementById("funcTable"+tableIter) != null; tableIter++)
    lastTable = document.getElementById("funcTable"+tableIter).getBoundingClientRect();

    var tags = document.getElementById("tags");
    var tagsWidth = 0;
    if (tags != null) { tagsWidth = tags.getBoundingClientRect().width; }

    var searchbox = 0
    if(document.getElementById("searchBox") != null)
    {
      searchbox = document.getElementById("searchBox").getBoundingClientRect().width;
    }
    var curOuterL = x/2 - searchbox/2 - tagsWidth;
    if(document.getElementById("tables")!= null){
      tables = document.getElementById("tables").getBoundingClientRect();
      curOuterL = ((x - tables.width) / 2) - tagsWidth + (lrdiff/2);
    }

    if( document.getElementById("fourfifth") != null){
      outergraph = document.getElementById("fourfifth").getBoundingClientRect();
    }

    screenMiddle = x/2;

    prevCur();

    var prevOuterL = screenMiddle - prevMiddle - tagsWidth - 26;
    var prevInnerL = prevMiddle - screenMiddle;

    var curInnerL = curMiddle - screenMiddle;

    if (curOuterL < 0) { outerLeft = 0; }
    else {
      outerLeft = curOuterL;
    }

    var innerLeft = 10000;
    var innerGraph = document.getElementById("inner-graph");
    if (curOuterL < 0) { innerLeft = - curOuterL; } else { innerLeft = 0; }

    // console.log("spaceColumn " + lastTable.right + " " + innerGraph.getBoundingClientRect().left + " " + innerGraph.getBoundingClientRect().right + " " + curMiddle + " " + screenMiddle);
    if ((lastTable.right - curMiddle) < screenMiddle) {
      var space = screenMiddle - (lastTable.right - innerGraph.getBoundingClientRect().left - curMiddle);
      if (document.getElementById("spaceColumn"))
      {
        document.getElementById("spaceColumn").style.width = "" + space + "px";
      }
      innerLeft = 10000;
    }

    var outergraph = document.getElementById("outer-graph");

    outergraph.addEventListener("animationend", listener, false);

    outergraph.style.position = "relative";
    if(state == "middle" || state == "search"){
    //  console.log("middle" + outerLeft + "  x" + x + "  tables.width" + tables.width);
      outergraph.style.left = "" + outerLeft + "px";
    }
    else {
    //  console.log("notMiddle"+ state);
      outergraph.style.left = "0px";
    }

    outergraph.scrollLeft = innerLeft;

    var expdiv = document.getElementById("expanded");
    expanded = expdiv.getAttribute("expanded");

    // console.log("moveTables2 " + prev + " " + current + " " + lastPrevAnim);
    if(expanded == "True")
    {
      // only animate when a graph is extended (to the left or right)
      if (prev.startsWith("Just") && current.startsWith("Just") && prev != current &&
          prev != lastPrevAnim)
      {
      // console.log("moveTables3 ");
        animationActive = true;
        lastPrevAnim = prev;
        var slide = curMiddle - prevMiddle

        if(document.getElementById("funcTable"+ nr) != null)
        {
          activeTable.style.width = document.getElementById("funcTable"+ nr).offsetWidth + "px";
        }
        console.log("nr " + nr + " " + document.getElementById("funcTable"+ nr).offsetWidth);

        insertStyleSheetRule("@keyframes example { 0% { width:"+slide+"px;} 100% { width:0px;} }");

        "use strict"; // This changes everything
        var element = document.getElementById("slideAnimColumn");
        if (element != null)
        {
          if (element.classList != null){ element.classList.remove("graph-animation"); }
          void element.offsetWidth;
          element.classList.add("graph-animation");
        }
      }
      document.getElementById("outer-graph").scrollLeft = 10000;
    } else
    {
       insertStyleSheetRule("@keyframes example { 0% { width:0px;} 100% { width:0px;} }");
       document.getElementById("outer-graph").scrollLeft = 0;
    }
  }
    //console.log("prM " + prevMiddle + "curM " + curMiddle + "  screenM " + screenMiddle + " prev " + prev + " " + current);
}


function nodeIsFunction(node){
  if(node == null){ return false; }
  else {
    return ((node.getAttribute("class") == "func node") ||
            (node.getAttribute("class") == "case node") ||
            (node.getAttribute("class") == "exefunc node") ||
            (node.getAttribute("class") == "arg node") ||
            (node.getAttribute("class") == "lit node") ||
            (node.getAttribute("class") == "meta node"));
  }
}

function nodeIsConnection(node){
  if(node == null){ return false; }
  else {
    return (node.getAttribute("class") == "connection");
  }
}

function connectNodes() {

  var connectionOffset = 10;
  var connectionXCutOff = 10;
  //console.log("connectNodes ");

  for(var tableIter=0; document.getElementById("funcTable"+tableIter) != null; tableIter++)
  {
    var table = document.getElementById("funcTable"+tableIter);
    var cols = table.children
    for(var col=1; col<cols.length; col++)
    {
      var cells = cols[col].children;
      var height = cells[0].offsetHeight-6;
      for(var cell=0; cell<cells.length; cell++)
      {
        var c = cells[cell];
        var d = c.lastChild;
        var xmlns = "http://www.w3.org/2000/svg";
//        console.log("cell " + cell + " " + c + d);
        if (d.nodeType == 1 && (d.getAttribute("class")) == "nodeConnect")
        {
          var nodes = d.firstChild.children;
          var svg = d.lastChild;
          while (svg.lastChild) {
            svg.removeChild(svg.lastChild);
          }

          var linesMin=10000;
          var linesMax=-10000;
          var maxLineWidth = 0;
          var moveSVG = 0;
          var caseDiff = 0;
          var caseMin = 10000;
          var caseArgUsed = false;

          for (var node=0; node<nodes.length; node++) // to calculate caseArgUsed
          {
              var from = nodes[node].getAttribute("from");
              var fromNode;
              if(from != "") { fromNode = document.getElementById(from); }
              var fromCell = nodes[node].getAttribute("fromCell");
              var fromNodeCell;
              if(from != "") { fromNodeCell = document.getElementById(fromCell); }
//              console.log("fromNode:" + fromNode.getAttribute("class") + " fromNodeCell:" + fromNodeCell.getAttribute("class"));
              if (fromNode != null && fromNodeCell != null &&
                  fromNode.getAttribute("class") == "arg node" && fromNodeCell.getAttribute("class") == "caseCell")
              {
                var fromRect = fromNode.getBoundingClientRect();
                if (fromRect.right < caseMin) { caseMin = fromRect.right; }
                caseArgUsed = true;
              }
          }

          if (!caseArgUsed) { caseMin = 0; }

          var leftMin = 0;
          var fromX;
          for (var node=0; node<nodes.length; node++) // iterate through node connect
          {
            var from = nodes[node].getAttribute("from");
            var fromNode; var toCellId;
            if(from != ""){ fromNode = document.getElementById(from);}

            if(fromNode != null){
              var fromCell = nodes[node].getAttribute("fromCell");
              var fromNodeCell;
              if(fromCell != "") { fromNodeCell = document.getElementById(fromCell);}
              var fromRect = fromNode.getBoundingClientRect();
              var fromCellRect = fromNodeCell.getBoundingClientRect();

              if (fromNodeCell.firstChild.getAttribute("class") == "inputs")
              {
                  fromX = fromRect.right - fromCellRect.right;
                  if (fromX < leftMin) { leftMin = fromX; }
                  //console.log("fromX " + fromX + "  fromRect.right " + fromRect.right + "  fromCellRect.right " + fromCellRect.right + " leftMin "+ leftMin);
              }

              var to = nodes[node].getAttribute("to");
              var toInput = nodes[node].getAttribute("to").slice(0, nodes[node].getAttribute("to").indexOf("input"));
              var toNode; var toNodeInput;
              if(to != "")     { toNode      = document.getElementById(to);}
              if(toInput != ""){ toNodeInput = document.getElementById(toInput);}
              var toCell   = nodes[node].getAttribute("toCell");
              var toNodeCell;
              if(toCell != ""){ toNodeCell = document.getElementById(toCell); }
              if(toCell != ""){ toCellId   = document.getElementById(toCell); }
              if(toNodeCell != null){
                var toCellRect = toNodeCell.getBoundingClientRect();
                if (toCellRect.top - fromCellRect.top < moveSVG){ moveSVG = toCellRect.top - fromCellRect.top; }
              }
              //console.log("#0 fromCell " + fromCell + "   toCell " + toCell + "   moveSVG " + moveSVG + "   toCellRect.top " + toCellRect.top + "   fromCellRect.top " + fromCellRect.top);

              var targetIsConnection;
              var targetIsFunction;
              if(toNode != null) {
                  targetIsFunction = nodeIsFunction(toNode);
                  targetIsConnection = nodeIsConnection(toNode);
              } else
              if(toNodeInput != null) {
                  targetIsFunction = nodeIsFunction(toNodeInput);
                  targetIsConnection = nodeIsConnection(toNodeInput);
              }

              var conni = 0;
              while(targetIsConnection && conni < 200) {
                conni++;
                //var toN = toNodeCell.nextSibling;
                var toN = toCellId.nextSibling;
                //console.log("toCell " + toCell + "  toCellId " + toCellId.innerHTML + "  toN " + toN);
                var connectTag = null;
                if(toN != null) { connectTag = toN.firstChild.firstChild; }
                //console.log("connectTag " + connectTag);
                //console.dir(connectTag);
                if(connectTag != null){
                  var connectToCell = connectTag.getAttribute("toCell");
                  var connectTo = connectTag.getAttribute("to");
                  var toNodeCell = document.getElementById(connectToCell);
                  var toNode     = document.getElementById(connectTo);

                  targetIsFunction = nodeIsFunction(toNode);
                  targetIsConnection = nodeIsConnection(toNode);
                  if(toNodeCell != null){
                    var toNodeCellRect = toNodeCell.getBoundingClientRect();
                    if (toNodeCellRect.top - fromCellRect.top < moveSVG){ moveSVG = toNodeCellRect.top - fromCellRect.top; }
                  }
                }
              }

              //console.log("#1 fromCell " + fromCell + "   toCell " + toCell + "   moveSVG " + moveSVG + "   toCellRect.top " + toCellRect.top + "   fromCellRect.top " + fromCellRect.top);
            }
          }

          for (var node=0; node<nodes.length; node++) // iterate through node connect
          {
            var path = document.createElementNS(xmlns, "path");
            //var polyline = document.createElementNS(xmlns, "polyline");

            var strokeWidth     = nodes[node].getAttribute("stroke-width");
            var stroke          = nodes[node].getAttribute("stroke"); // colour
            var opacity         = nodes[node].getAttribute("opacity");
            var strokeDasharray = nodes[node].getAttribute("stroke-dasharray");

            path.setAttributeNS(null,"stroke-width", strokeWidth);
            path.setAttributeNS(null,"stroke", stroke); // colour
            path.setAttributeNS(null,"stroke-opacity", opacity);
            path.setAttributeNS(null,"stroke-dasharray", strokeDasharray);
            path.setAttributeNS(null, "style",
              "fill:none;fill-rule:evenodd;stroke-linecap:round;stroke-linejoin:round;stroke-miterlimit:3;");
            svg.setAttributeNS(null,"z-index", "-1");

            svg.appendChild(path);
            //svg.appendChild(polyline);

            var from     = nodes[node].getAttribute("from");
            var fromCell = nodes[node].getAttribute("fromCell");
            var to = nodes[node].getAttribute("to");
            var toInput = nodes[node].getAttribute("to").slice(0, nodes[node].getAttribute("to").indexOf("input"));
            var toCell   = nodes[node].getAttribute("toCell");

            var fromNode; var toNode; var toNodeInput; var fromNodeCell; var toCellId;
            if(from != "")   { fromNode    = document.getElementById(from);}
            // to 583input0 toInput 583
            if(to != "")     { toNode      = document.getElementById(to);}
            if(toInput != ""){ toNodeInput = document.getElementById(toInput);}

            if(fromCell != ""){ fromNodeCell = document.getElementById(fromCell);}
            if(toCell != "")  { toCellId   = document.getElementById(toCell);}

            if ((toNode != null || toNodeInput != null) && fromNode != null && (fromNode.getAttribute("class") != "connection"))
            {
              var sourceIsFunction = nodeIsFunction(fromNode);
              var sourceIsConnection = nodeIsConnection(fromNode);

              var targetIsConnection;
              var targetIsFunction;
              if(toNode != null) {
                  targetIsFunction = nodeIsFunction(toNode);
                  targetIsConnection = nodeIsConnection(toNode);
              } else
              if(toNodeInput != null) {
                  targetIsFunction = nodeIsFunction(toNodeInput);
                  targetIsConnection = nodeIsConnection(toNodeInput);
              }

              var connNodes = [];
              var conni = 0;
              while(targetIsConnection && conni < 200) {
                  conni++;
                  if(toNode != null) {
                    var nodeAndCell = {
                      "node": toNode,
                      "cell": toCellId
                    }
                    connNodes.push(nodeAndCell);
                  }
                  else { if(toNodeInput != null) {
                    var nodeAndCell = {
                      "node": toNodeInput,
                      "cell": toCellId
                    }
                    connNodes.push(nodeAndCell); toNodeInput = null;}
                  }
                  var toN = toCellId.nextSibling;
                  var toN2 = toCellId.firstChild.nextSibling;

                  var connectTag = null;
                  if(toN != null) { connectTag = toN.firstChild.firstChild; }
                  var connectTag2 = toCellId?.firstChild?.nextSibling?.firstChild?.firstChild ?? connectTag;
                  var tagto = connectTag2.getAttribute("toCell");
                  var toto = connectTag2.getAttribute("to");
                  var toCellId = document.getElementById(connectTag2.getAttribute("toCell"));
                  var toNode     = document.getElementById(connectTag2.getAttribute("to"));

                  targetIsFunction = nodeIsFunction(toNode);
                  targetIsConnection = nodeIsConnection(toNode);

                  if (fromCellRect.top < linesMin){ linesMin = fromCellRect.top; }
                  if (fromCellRect.bottom > linesMax){ linesMax = fromCellRect.bottom; }
                  if(toCellId != null){
                    var toCellRect = toCellId.getBoundingClientRect();
                    if (toCellRect.top < linesMin){ linesMin = toCellRect.top; }
                    if (toCellRect.bottom > linesMax){ linesMax = toCellRect.bottom; }
                  }
              }

              var fromRect = fromNode.getBoundingClientRect();
              var toRect;
              if(toNode != null)           { toRect = toNode.getBoundingClientRect(); }
              else if(toNodeInput != null) { toRect = toNodeInput.getBoundingClientRect(); }
              var fromCellRect = fromNodeCell.getBoundingClientRect();
              if(sourceIsConnection){
                  var lineWidth = toRect.left - fromRect.right + connectionXCutOff;
              }
              else {
                  var lineWidth = toRect.left - fromRect.right;
              }
              if(lineWidth > maxLineWidth){ maxLineWidth = lineWidth; }
              if(lineWidth < 0) { lineWidth = -lineWidth; }
              if(maxLineWidth < 3) { maxLineWidth = 3; }

              var startX = 0;
              if (fromNode.getAttribute("class") == "arg node" && fromNodeCell.getAttribute("class") == "caseCell")
              {
                startX = fromRect.right - caseMin;
              }

              if (fromCellRect.top < linesMin){ linesMin = fromCellRect.top; }
              if (fromCellRect.bottom > linesMax){ linesMax = fromCellRect.bottom; }
              if(toCellId != null){
                var toCellRect = toCellId.getBoundingClientRect();
                if (toCellRect.top < linesMin){ linesMin = toCellRect.top; }
                if (toCellRect.bottom > linesMax){ linesMax = toCellRect.bottom; }
              }

              var lh = linesMax - linesMin;
              var lineHeight;
              if(Math.abs(moveSVG) > lh){ lineHeight = Math.abs(moveSVG) + lh; } else { lineHeight = lh; }

              svg.setAttribute("width", "" + (maxLineWidth + connectionXCutOff*2));
              svg.setAttribute("height", "" + lineHeight);

              var moveSVGStr = " top: " + moveSVG + "px;";

              var fromY;
              var offsetX = 0;
              if (caseArgUsed)
              {
                  fromX = caseMin - fromCellRect.right;
                  fromY = fromRect.height/2 + (fromRect.top - fromCellRect.top);
                  svg.setAttribute("style","position:absolute;display: block;left:"+ fromX +"px;"+ moveSVGStr);
                  //console.log("from " + from + "   fromCell " + fromCell);
                  //console.log("fromY " + fromY + "   fromRect.height " + fromRect.height + "   fromRect.top " + fromRect.top + "   fromCellRect.top " + fromCellRect.top);
              } else
              if (sourceIsFunction)
              {
                  fromX = fromRect.right - fromCellRect.right;
                  fromY = fromRect.height/2 + (fromRect.top - fromCellRect.top);
                  svg.setAttribute("style","position:absolute;display: block;left:"+ fromX +"px;"+ moveSVGStr);
                  //console.log("from to " + from + " " + to + " isFunction fromRect.height/2 " + fromRect.height/2 + " fromCellRect.height/2 " + fromCellRect.height/2);
              } else
              if (sourceIsConnection)
              {
                  fromX = fromRect.right - fromCellRect.right - connectionXCutOff;
                  fromY = fromRect.height/2 + (fromRect.top - fromCellRect.top) + connectionOffset - hack;
                  svg.setAttribute("style","position:absolute;display: block;left:"+ fromX +"px;"+ moveSVGStr);
                  //console.log("connection " + fromX);
              } else if (fromNodeCell.firstChild.getAttribute("class") == "inputs")
              {
                  offsetX = fromRect.right - fromCellRect.right - leftMin;
                  //console.log("from " + from + "   leftMin " + leftMin + "   offsetX " + offsetX);
                  fromY = (fromRect.height - 1.5) + (fromRect.top - fromCellRect.top);
                  svg.setAttribute("style","position:absolute;display: block;left:"+ leftMin +"px;"+ moveSVGStr);
              } else
              {
                  fromX = 0;
                  fromY = (fromRect.height - 1.5) + (fromRect.top - fromCellRect.top);
                  svg.setAttribute("style","position:absolute;display: block;left:"+ fromX +"px;"+ moveSVGStr);
                  //console.log("else " + fromX);
              }

              var toX;
              var toY;
              var hack = 1.8;
              if (targetIsFunction)
              {
                  toX = lineWidth;
                  toY = toRect.height/2 + toCellRect.top - fromCellRect.top
                                        + (toRect.top - toCellRect.top);
              //    console.log("targetIsFunction " + toRect.top + " " + toCellRect.top + " " + toY);
              } else
              if (targetIsConnection)
              {
                  toX = lineWidth + connectionXCutOff;
                  toY = toRect.height/2 + toRect.top - fromCellRect.top + connectionOffset - hack;
              } else
              {
                  toX = lineWidth;
                  toY = (toRect.height - 1.5) + (toRect.top - fromCellRect.top);
              }

              var x = startX;
              var y = fromY - moveSVG;
              let points = [];

              function pathPoint(p, index, array) {
                if(p != null){
                  var pRect     = p.node.getBoundingClientRect();
                  var pCellRect = p.cell.getBoundingClientRect();
                  cx = pRect.right - fromRect.right - pRect.width/2;
                  cy = pCellRect.height/2 + pCellRect.top - fromCellRect.top + connectionOffset - hack - moveSVG;
                  //console.log("cx " + cx + "   cy " + cy + "   index " + index + "   node " + node);
                  //console.log("fromCellRect.height " + fromCellRect.height + "\npRect.height " + pRect.height + "\npCellRect.height " + pCellRect.height + "\nconne-hack-move " + (connectionOffset - hack - moveSVG));
                  //console.log("fromCellRect.top " + fromCellRect.top + "\ntoRect.top " + toRect.top + "\npRect.top " + pRect.top + "\npCellRect.top " + pCellRect.top);
                  let point = {
                    "x": cx,
                    "y": cy
                  }
                  points.push(point);
                }
              }

              let firstPoint = {
                    "x": (x + offsetX),
                    "y": y
                  }
              points.push(firstPoint);

              connNodes.forEach(pathPoint);

              let lastPoint = {
                    "x": (x + toX + offsetX),
                    "y": (toY - moveSVG)
                  }
              points.push(lastPoint);

              //if(connNodes.length > 0){ console.log(connNodes); }
              //console.log(points);
              var insertTang = false;
              let tangStart = {
                "x": (x + offsetX + ((toX - x)/points.length)),
                "y": y
              }

              // if two points are not at the same y-position, insert a helper point that produces a horizontal starting direction
              if(Math.abs(points.at(0).y - points.at(1).y) > 5){
                  insertTang = true;
                  //console.log("tangStart" + tangStart.x + " " + tangStart.y);
              }

              // if two points are not at the same y-position, insert a helper point that produces a horizontal ending direction
              if(Math.abs(points.at(points.length-2).y - points.at(points.length-1).y) > 5){
                  let tangEnd = {
                    "x": (x + offsetX + (toX - x)*(points.length/(points.length+1))),
                    "y": (toY - moveSVG)
                  }
                  points.splice(points.length-1, 0, tangEnd);
                  //console.log("tangEnd" + + tangEnd.x + " " + tangEnd.y);
              }
              if(insertTang){ points.splice(1, 0, tangStart); }
              //console.log(points);

              bezierCurves = "";
              for (let i = 1; i < points.length; i+=3) {
                  if(i+2 < points.length){
                      bezierCurves += "C " + points.at(i).x + "," + points.at(i).y + " " +
                                             points.at(i+1).x + "," + points.at(i+2).y + " " +
                                             points.at(i+2).x + "," + points.at(i+2).y;
                  } else
                  if(i+2 == points.length){
                    if(i==1){ bezierCurves += "Q " + points.at(i).x + "," + points.at(i).y + " " +
                                                     points.at(i+1).x + "," + points.at(i+1).y; }
                    else { bezierCurves += "S " + points.at(i).x + "," + points.at(i).y + " " +
                                                  points.at(i+1).x + "," + points.at(i+1).y; }
                  } else
                  if (i+1 == points.length){
                      bezierCurves += "S " + (points.at(i).x-((toX - x)/points.length)) + "," + points.at(i).y + " " +
                                             points.at(i).x + "," + points.at(i).y + " ";
                  }
              }
              var pline = "";
              for (let i = 1; i < points.length; i++) { pline += " " + points.at(i).x + "," + points.at(i).y; }

              var d = "M"  + (x + offsetX) + "," + y + bezierCurves;
              //console.log(d);
              path.setAttribute("d", d);
              //polyline.setAttribute("points", pline);
              //polyline.setAttribute("style", "fill:none");
            }
          }
        }
      }
    }
  }
}

var blocked = false;

function handleSummary(summaries) {
  blocked = true;
  setHeightsOfColumnCells();
  moveTables();
  connectNodes();
  //console.log("handleSummary ");
}

function setupWatch() {

  var observer = new MutationObserver(function(mutations) {
    if (blocked) {
      blocked = false;
      return;
    }

    setTimeout(handleSummary, 120); handleSummary();
    // setTimeout(handleSummary, 100); handleSummary();
  });

  var observerConfig = {
       attributes: true
    ,  childList: true
    ,  subtree: true
    ,  characterData: true
  };

  var targetNode = document.body; // document.getElementById("inner-graph");
  observer.observe(targetNode, observerConfig);
}

window.addEventListener("DOMContentLoaded", setupWatch);
window.addEventListener("resize", handleSummary);

