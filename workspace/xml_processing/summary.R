library(XML)

setwd("~/Documents/UCDavis/2014Winter/STA250/HW4/workspace/data")

data_RSSI_raw = read.csv("omni_16dbm.csv", header = FALSE, sep = "\t", as.is=TRUE) # Import the RSSI file

# Function to create a list of lists based on dataframe corresponding to observations at one location on one specific direction
create_list_per_direction <- function(frame_per_direction) {
	list_per_direction = list(list(mean = mean(frame_per_direction$V7), sd = sd(frame_per_direction$V7), ID = 1),
				  list(mean = mean(frame_per_direction$V8), sd = sd(frame_per_direction$V8), ID = 2),
				  list(mean = mean(frame_per_direction$V9), sd = sd(frame_per_direction$V9), ID = 3),
				  list(mean = mean(frame_per_direction$V10), sd = sd(frame_per_direction$V10), ID = 4),
				  list(mean = mean(frame_per_direction$V11), sd = sd(frame_per_direction$V11), ID = 5))
	list_per_direction$number_sample = nrow(frame_per_direction)
	list_per_direction$direction = frame_per_direction$V4[1]
	return(list_per_direction)
}

# Function to create a list of lists based on dataframe corresponding to observations at one location
create_list_per_location <- function(frame_per_location) {
	list_per_location = list()
	list_per_location$location = c(frame_per_location$V1[1], frame_per_location$V2[1])

	frame_per_direction = split(frame_per_location, frame_per_location$V4)
	
	list_per_location$list_per_direction = lapply(frame_per_direction, create_list_per_direction)
	return(list_per_location)
}

# Function to create a list of lists in the same structure as the desired XML file
create_list <- function(frame_data) {
	frame_per_location = split(frame_data, list(frame_data$V1, frame_data$V2), drop=TRUE)
	list_data = lapply(frame_per_location, create_list_per_location)
	return(list_data)
}

list_RSSI = create_list(data_RSSI_raw)

# Create the XML file
node_root = newXMLNode("root")


# Add RSSI statistics to the node_root, handle the inconsistence in the original data (Create node only for the complete branches)
create_node_BS <- function(list_per_BS, node_direction) {
	node_BS = newXMLNode(paste("BS", list_per_BS$ID, sep = "", collapse = NULL), parent = node_direction)
	node_mean = newXMLNode("mean", parent = node_BS)
	node_sd = newXMLNode("sd", parent = node_BS)
	node_text_mean = newXMLTextNode(list_per_BS$mean, parent = node_mean)
	node_text_sd = newXMLTextNode(list_per_BS$sd, parent = node_sd)
}


create_node_direction <- function(list_per_direction, node_location) {	
	node_direction = newXMLNode(list_per_direction$direction, parent = node_location)
	lapply(list_per_direction[1 : 5], create_node_BS, node_direction = node_direction)
}

create_node_location <- function(list_per_location, node_RSSI) {
	if (length(list_per_location$list_per_direction) == 4) {
		node_location = newXMLNode("location", parent = node_RSSI)
		node_x = newXMLNode("x", parent = node_location)
		node_text_x = newXMLTextNode(list_per_location$location[1], parent = node_x)
		node_y = newXMLNode("y", parent = node_location)
		node_text_y = newXMLTextNode(list_per_location$location[2], parent = node_y)

		lapply(list_per_location$list_per_direction, create_node_direction, node_location = node_location)
	}
}

create_node_RSSI <- function(list_RSSI, node_root) {

	node_RSSI = newXMLNode("RSSI", parent = node_root)
	lapply(list_RSSI, create_node_location, node_RSSI = node_RSSI)
}

create_node_RSSI(list_RSSI, node_root)

# Add floorplan information to the node_root
floorplan_corner = read.csv("floorplan_corner.csv", header = FALSE, sep = "\t", as.is=TRUE)
floorplan_wall = read.csv("floorplan_wall.csv", header = FALSE, sep = "\t", as.is=TRUE)

node_floorplan = newXMLNode("floorplan", parent = node_root)
node_list_corner = newXMLNode("list_corner", parent = node_floorplan)
node_list_wall = newXMLNode("list_wall", parent = node_floorplan)

create_node_corner <- function(vec_row, node_list_corner) {
	node_corner = newXMLNode("corner", parent = node_list_corner)

	node_ID = newXMLNode("ID", parent = node_corner)
	node_text_ID = newXMLTextNode(vec_row[1], parent = node_ID)
	node_x = newXMLNode("x", parent = node_corner)
	node_text_x = newXMLTextNode(vec_row[2], parent = node_x)
	node_y = newXMLNode("y", parent = node_corner)
	node_text_y = newXMLTextNode(vec_row[3], parent = node_y)
}

create_node_wall <- function(vec_row, node_list_wall) {
	node_wall = newXMLNode("wall", parent = node_list_wall)

	node_ID_begin = newXMLNode("ID_corner_begin", parent = node_wall)
	node_text_ID_begin = newXMLTextNode(vec_row[1], parent = node_ID_begin)
	node_ID_end = newXMLNode("ID_corner_end", parent = node_wall)
	node_text_ID_end = newXMLTextNode(vec_row[2], parent = node_ID_end)
}

apply(floorplan_corner, 1, create_node_corner, node_list_corner = node_list_corner)
apply(floorplan_wall, 1, create_node_wall, node_list_wall = node_list_wall)

# Add BS information
BS = read.csv("position_BS.csv", header = FALSE, sep = "\t", as.is=TRUE)

node_BS = newXMLNode("BS", parent = node_root)

create_node_position_BS <- function(vec_row, node_BS) {
	node_position_BS = newXMLNode(paste("BS", vec_row[1], sep = "", collapse = NULL), parent = node_BS)

	node_x = newXMLNode("x", parent = node_position_BS)
	node_text_x = newXMLTextNode(vec_row[2], parent = node_x)
	node_y = newXMLNode("y", parent = node_position_BS)
	node_text_y = newXMLTextNode(vec_row[3], parent = node_y)
}
apply(BS, 1, create_node_position_BS, node_BS = node_BS)


saveXML(node_root, file = "data_RSSI.xml")
