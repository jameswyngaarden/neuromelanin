#!/bin/tcsh


#######################################################
# SCRIPT SUMMARY
#######################################################
# This scipt was written by Johanna Jarcho to:
# - extract activity from specified anatomical ROIs during specified events

# JW (3/17/21)
# tcsh -xef nm_jw_RoiExtraction.sh |& tee nm_jw_RoiExtraction.output

############################################################################################
# GENERAL SETUP
############################################################################################

# Specify interum date that data were collected through or sample sizes
set collection = 060221

# IMPORTANT NOTE: this script makes a list of everyone who has a flder in IndvlLvlAnalysis, and pulls data from those people
# - TO ENSURE YOU PULL THE CORRECT PEOPLE FOR YOUR ANALYSIS, MODIFY YOUR LIST MANUALLY, OR TEMPORARILY MOVE DATA FROM INDVLVLANALYSES

######################################################
# SPECIFY YOUR LOCATIONS AND INFORMATION ABOUT YOUR DATA
######################################################

# Specify the top-most directory associated with the study
set topdir = /data/projects/STUDIES/Social_Doors/TaskVersion_4Runs/Social_Doors_CollegeSBU/fMRI_ANALYSES

# Specify the where individual level analyses are found
set indvlvldata = $topdir/IndvlLvlAnalyses

# Specify the suffix for the individual level directory where your subject's stats files are located
set subjresults = results.social.1mm
#set subjresults = results.doors.1mm

## CHANGE ME:  Specify the analysis of interest (e.g., anticipation or feedback with which individual level data and date)
#set analysis = FB.SocialDoors.111319
#set analysis = Ant.SocialDoors.111319
#set analysis = FB.Doors.111319
#set analysis = 3dMVM.FB.Doors.NMvstri.080420
set analysis = 3dMVM.FB.Social_PositivexNMfull_NonInterp_070321 

#set atlasdir = /Volumes/SDN_Data/STUDIES/AFNI_ROI_Atlases/TT_Daemon_2.5x2.5x2.5
#set atlasdir = /Volumes/SDN_Data/AnatomicalROI_Masks/ROIs/TT_N27_EZ_ML
set atlasdir = /data/projects/STUDIES/Social_Doors/TaskVersion_4Runs/Social_Doors_CollegeSBU/fMRI_ANALYSES/MASKS/Anatomical

######################################################
# SPECIFY INFORMATION ABOUT YOUR ROI
######################################################

# Specify type of ROI mask (Anatomical or ActivationBased)
set ROItype = Anatomical

## CHANGE ME: Specify the name of the anatomical ROI of interest

#TT N27 EZ ML
#set ROIs = ( Anterior_Cingulate_Cortex Caudate_Nucleus Fusiform_Gyrus Middle_Cingulate_Cortex Middle_Frontal_Gyrus Middle_Orbital_Gyrus Middle_Temporal_Gyrus Pallidum Posterior_Cingulate_Cortex Superior_Temporal_Gyrus Temporal_Pole Thalamus )


#TT Daemon
set ROIs = ( dorsalStriatum ventralStriatum_28jun21 ) 
#set ROIs = ( Amygdala Caudate_Body Caudate_Head Caudate_Tail Caudate Anterior_Cingulate Putamen Posterior_Cingulate Insula Nucleus_Accumbens Insula Precuneus VS_6mm )
#set ROIs = ( dacc )
#set ROIs = ( MedialFrontalGyrus_01 MiddleTemporalGyrus_01 PostCentralGyrus_01 Putamen_03 SuperiorParietalLobule_01 )
#set ROIs = ( Amygdala_01 CingulateGyrus_01 Claustrum_01 )
#set ROIs = ( NMvstri:Task:Outcome.clust15 )

# CHANGE ME: Specify laterality of the ROIs of interest (left = l, right = r)
#tt daemon
set ROIhemispheres = ( l )

#tt n27 ez ml
#set ROIhemispheres = ( L )

######################################################
# SPECIFY INFORMATION ABOUT YOUR ROI EXTRACTION OUTPUT
# NOTE: much of this information comes from your individual level analyses
######################################################


# Specify the sub-brik numbers at the individual level analysis that correspond to the coefficients you used to generate this group-level analysis

#set stim_list = ( 01 04 )
#set stim_list = ( 07 10 13 16 )
set stim_list = ( 13 16 )

#Doors
#LoseGames Ant 01
#WinGames Ant 04
#LoseGames FB Lose 07 
#LoseGames FB Win 10
#WinGames FB Lose 13
#WinGames FB Win 16

#Social Doors
#Dislike Ant 01
#Like Ant 04
#Dislike FB Lose 07 
#Dislike FB Win 10
#Like FB Lose 13
#Like FB Win 16

############################################################################################
# THE SCRIPT BEGINS HERE
############################################################################################

################################################################
# FINISH SETUP
################################################################


# SET the ROI output directory for your analysis
set ROIdir = $topdir/MASKS/$ROItype

# Make a directory for the specific analysis you are making masks for if it does not exist
	  # If this directory (-d) does exists, then...
	if ( -d $ROIdir/$analysis ) then
		# print this statement
		echo "$ROIdir/$analysis already exists"
		# if it doesn't exist, create it
	 	else 
		mkdir $ROIdir/$analysis
	endif


# Generate a list of subjects that will be included in analyses if it does not yet exist
cd $indvlvldata
	# If this file (-f) exist, then...
	if ( -f $indvlvldata/subjList.ROI.$collection ) then
		# print this statement
		echo "$indvlvldata/subjList.ROI.$collection already exists"
		# if it doesn't exist, create it		
	 	#else 
		#echo s* |& tee $indvlvldata/SubjList.ROI.$collection
	endif


# Perfom a series of commands for each hemisphere you are interested in
foreach ROIhemisphere ( $ROIhemispheres )

	# Perform a series of commands for each of your ROIs
	foreach ROI ( $ROIs )
		
		# SET the name of the output file for your data
		set SPSSfile = $ROIdir/$analysis/SPSS.$ROI.$ROIhemisphere.txt
	
	######################################################
	# USE 3DROISTAT AND CODE WRITTEN BY RICK REYNOLDS TO:
	# EXRACT DATA FROM EACH CLUSTER MASK AND COMPILE IT IN AN SPSS-FRIENDLY FORMAT TXT FILE
	######################################################
	
	
	# Run ROI extraction
	# For each ROI specified above, extract data:
	# for each subject  
	# for each stimulus-specified contrast 
	# ...then move on to the next ROI  ###
	
	# ----------------------------------------------------------------------
		# Clear any pre-existing, old SPSS output file
		echo -n "" > $SPSSfile
	
	# ----------------------------------------------------------------------
		# create a header for the SPSS FILE using a representative single subject from one group
		set SPSSheader = ( Subject )
		cd $indvlvldata
	
		# Indicate the list of subjects you will be using
		set subjects = (`cat subjList.ROI.$collection`)
		set subj = $subjects[1]
	
		# create a line of the SPSS header
	
		foreach stim ($stim_list)
			### Set input file (one for each contrast for each subject) ###
			set infile = $subj/$subj.$subjresults
		
			# get header only, skipping 'file' and 'sub-brick'
			
			#TT N27 EZ ML
			#set headers = ( `3dROIstats -nzmean -nzminmax -nzvoxels  \
			#		 	    -mask $atlasdir/$ROI'_'$ROIhemisphere+tlrc $infile/'stats.'$subj'+tlrc['$stim']' | head -n 1` )
								
			#set headers = ( `3dROIstats -nzmean -nzminmax -nzvoxels  \
			#		-mask $atlasdir/$ROI+tlrc $infile/'stats.'$subj'+tlrc['$stim']' | head -n 1` )
				
			#TT Daemon
			#set headers = ( `3dROIstats -nzmean -nzminmax -nzvoxels  \
			#		 	    -mask $atlasdir/$ROI'_'$ROIhemisphere"_2.5x2.5x2.5"+tlrc $infile/'stats.'$subj'+tlrc['$stim']' | head -n 1` )
		
			set headers = ( `3dROIstats -nzmean -nzminmax -nzvoxels  \
					 	    -mask $atlasdir/$ROI'_'$ROIhemisphere+tlrc $infile/'stats.'$subj'+tlrc['$stim']' | head -n 1` )

			# append the header elements (skipping first 2) and include stimulus name
			foreach item ( $headers[3-] )
				set SPSSheader = ( $SPSSheader $stim.$item )
			end
	#end
	
		cd ../../..
	
		# init SPSS file with header
		echo $SPSSheader > $SPSSfile
				
	# ----------------------------------------------------------------------
	# add all the stats
		cd $indvlvldata
	
		set subjects = (`cat subjList.ROI.$collection`)
		foreach subj ( $subjects )
	
			# create one line of the SPSS file for each subject in the subject list
			echo "subject $subj ...."
	
			set subj_stats = ()
			foreach stim ( $stim_list )
				
				   ### Set input file (one for each contrast for each subject) ###
				   set infile = $subj/$subj.$subjresults
	
				   ### Run extraction using 3dROIstats (-nz options output additional non-zero data).
				   
				   #TT N27 EZ ML
				   #set stats = ( `3dROIstats -quiet -nzmean -nzminmax -nzvoxels  \
				  #			  -mask $atlasdir/$ROI'_'$ROIhemisphere+tlrc $infile/'stats.'$subj'+tlrc['$stim']'` )
								  
			   		#set stats = ( `3dROIstats -quiet -nzmean -nzminmax -nzvoxels  \
			   		#		  -mask $atlasdir/$ROI+tlrc $infile/'stats.'$subj'+tlrc['$stim']'` )
								  
					#TT Daemon
					#set stats = ( `3dROIstats -quiet -nzmean -nzminmax -nzvoxels  \
			   		#		  -mask $atlasdir/$ROI'_'$ROIhemisphere"_2.5x2.5x2.5"+tlrc $infile/'stats.'$subj'+tlrc['$stim']'` )
	
					set stats = ( `3dROIstats -quiet -nzmean -nzminmax -nzvoxels  \
			   				  -mask $atlasdir/$ROI'_'$ROIhemisphere+tlrc $infile/'stats.'$subj'+tlrc['$stim']'` )
				
				   # append stats for this subject
				   set subj_stats = ( $subj_stats $stats )
	
			end
	
			# Populate the SPSS file by appending subject ID, and, ROI stats for each subject to an output file (spssfile)
			echo $subj $subj_stats >> $SPSSfile
	
		end
	
		cd ../..
	
	end
	
	end
	
end	
	
