library(XML)
library(SVGAnnotation)
setwd("~/Documents/UCDavis/2014Winter/STA250/HW4/workspace/data")

svg_root = xmlRoot(xmlTreeParse("testVis.svg", useInternalNodes=TRUE))

# Add javascript to the svg file
script_display_direction = newXMLNode("script", parent = svg_root)
addAttributes(script_display_direction, "ID"="display_dirction", "xmlns:xlink"="http://www.w3.org/1999/xlink", "xmlns"="http://www.w3.org/2000/svg", "type"="text/javascript")

text_display_direction = "function showDirection(obj)
				{
      					if(typeof lineGroup != 'undefined') 
      					{
            					lineGroup.parentNode.removeChild(lineGroup);
      					}
      					var x = parseFloat(obj.getAttribute('cx'));
      					var y = parseFloat(obj.getAttribute('cy'));

      					lineGroup = document.createElementNS('http://www.w3.org/2000/svg','g'); 
            				lineGroup.setAttribute('idname', obj.getAttribute('id'));
      					obj.parentNode.appendChild(lineGroup);

      					var line1 = document.createElementNS('http://www.w3.org/2000/svg', 'line'); 
      					line1.setAttribute('x1', x);
      					line1.setAttribute('y1', y);
      					line1.setAttribute('x2', x);
      					line1.setAttribute('y2', y - 15);
      					line1.setAttribute('style', 'fill: blue; stroke: blue; stroke-width: 3');
      					line1.setAttribute('id', 'up');
      					line1.setAttribute('onmouseover', 'showRSSI(this)');
      					lineGroup.appendChild(line1);
		
					var line2 = document.createElementNS('http://www.w3.org/2000/svg', 'line'); 
      					line2.setAttribute('x1', x);
      					line2.setAttribute('y1', y);
      					line2.setAttribute('x2', x - 15);
      					line2.setAttribute('y2', y);
      					line2.setAttribute('style', 'fill: blue; stroke: blue; stroke-width: 3');
      					line2.setAttribute('id', 'left');
      					line2.setAttribute('onmouseover', 'showRSSI(this)');
      					lineGroup.appendChild(line2);
			
      					var line3 = document.createElementNS('http://www.w3.org/2000/svg', 'line'); 
      					line3.setAttribute('x1', x);
      					line3.setAttribute('y1', y);
      					line3.setAttribute('x2', x);
      					line3.setAttribute('y2', y + 15);
      					line3.setAttribute('style', 'fill: blue; stroke: blue; stroke-width: 3');
      					line3.setAttribute('id', 'down');
      					line3.setAttribute('onmouseover', 'showRSSI(this)');
      					lineGroup.appendChild(line3);

      					var line4 = document.createElementNS('http://www.w3.org/2000/svg', 'line'); 
      					line4.setAttribute('x1', x);
      					line4.setAttribute('y1', y);
      					line4.setAttribute('x2', x + 15);
      					line4.setAttribute('y2', y);
      					line4.setAttribute('style', 'fill: blue; stroke: blue; stroke-width: 3');
      					line4.setAttribute('id', 'right');
      					line4.setAttribute('onmouseover', 'showRSSI(this)');
      					lineGroup.appendChild(line4);

      					var connection = new Array();
      					for (var i=0;i<5;i++)
      					{
            					connection[i] = document.createElementNS('http://www.w3.org/2000/svg', 'line');
            					connection[i].setAttribute('x1', x);
            					connection[i].setAttribute('y1', y);
				                var textBS='BS';
            					var nodeBS = document.getElementById(textBS.concat((i+1).toString()));
            					var coordinate = nodeBS.getAttribute('points');
            					coordinate = coordinate.split(/[ ,]+/);
            					connection[i].setAttribute('x2', coordinate[0]);
            					connection[i].setAttribute('y2', coordinate[1]);
            					connection[i].setAttribute('style', 'fill: black; stroke: black; stroke-width: 1');
      					        connection[i].setAttribute('id', i.toString());
				            	lineGroup.appendChild(connection[i]);      						
      					}
				}

				function showRSSI(obj)
				{
            				if(typeof labelGroup != 'undefined') 
      					{
            					labelGroup.parentNode.removeChild(labelGroup);
      					}
      					labelGroup = document.createElementNS('http://www.w3.org/2000/svg','g'); 
      					obj.parentNode.appendChild(labelGroup);
      					var circleNode = document.getElementById(obj.parentNode.getAttribute('idname'));
      					var directionNode = circleNode.getElementsByTagName(obj.getAttribute('id'))[0]
      					var label = new Array();
      					var labelText = new Array();
      					for (var i=0;i<5;i++)
      					{
      						label[i] = document.createElementNS('http://www.w3.org/2000/svg', 'text');
            					labelText[i] = document.createTextNode(Math.round(parseFloat(directionNode.children[i].children[0].childNodes[0].nodeValue)).toString());
            					label[i].appendChild(labelText[i]);
            					var xMid = (parseFloat(document.getElementById(i.toString()).getAttribute('x1')) + parseFloat(document.getElementById(i.toString()).getAttribute('x2'))) / 2;
            					var yMid = (parseFloat(document.getElementById(i.toString()).getAttribute('y1')) + parseFloat(document.getElementById(i.toString()).getAttribute('y2'))) / 2;
            					label[i].setAttribute('x', xMid.toString());
           					label[i].setAttribute('y', yMid.toString());
           					label[i].setAttribute('fill', 'blue');
            					labelGroup.appendChild(label[i]);      
      					}
				}
"

newXMLTextNode(text_display_direction, parent = script_display_direction)

circles = xmlChildren(svg_root[[3]])
sapply(seq(along = circles), 
       function(i) { 
        addAttributes(circles[[i]], 
                      onclick = 
                        "showDirection(this)") 
       }) 

saveXML(svg_root, file = "testVisAnnotated.svg")
