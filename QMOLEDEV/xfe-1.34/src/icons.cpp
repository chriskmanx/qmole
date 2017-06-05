// Global icons for all applications

#include "config.h"
#include "i18n.h"

#include <fx.h>
#include <FXPNGIcon.h>

#include "xfedefs.h"
#include "icons.h"


// Icons (global variables)

FXIcon *archaddicon, *archexticon, *attribicon, *bigblockdevicon, *bigbrokenlinkicon, *bigcdromicon, *bigchardevicon;
FXIcon *bigdocicon, *bigexecicon, *bigfloppyicon, *bigfolderlockedicon, *bigfolderopenicon;
FXIcon *bigfoldericon, *bigfolderupicon, *bigharddiskicon, *bigiconsicon, *biglinkicon, *bignfsdriveicon;
FXIcon *bignfsdriveumticon, *bigpipeicon, *bigsocketicon, *bigzipicon, *cdromicon;
FXIcon *closefileicon, *clrbookicon, *collfoldericon, *copy_bigicon, *colltreeicon;
FXIcon *copy_clpicon, *cut_clpicon, *delete_big_permicon, *delete_bigicon, *deselicon, *detailsicon;
FXIcon *dirupicon, *editicon, *entericon, *errorbigicon, *exptreeicon;
FXIcon *filedelete_permicon, *filedeleteicon, *fileopenicon;
FXIcon *viewicon, *filtericon, *find_againicon, *fliplricon, *flipudicon, *floppyicon;
FXIcon *fontsicon, *gotobigicon, *gotolineicon, *harddiskicon, *helpicon, *hidehiddenicon;
FXIcon *hidenumbersicon, *hidethumbicon, *homeicon, *infobigicon, *invselicon, *link_bigicon;
FXIcon *locationicon, *lowercaseicon, *maphosticon, *miniappicon, *miniblockdevicon, *minibrokenlinkicon;
FXIcon *minichardevicon, *minidocicon, *miniexecicon, *minifolderclosedicon;
FXIcon *minifolderlockedicon, *minifolderopenicon, *minifoldericon, *minifolderupicon, *minilinkicon;
FXIcon *minipipeicon, *minishellicon, *minisocketicon;
FXIcon *move_bigicon, *moveiticon, *newfileicon, *newfoldericon, *nfsdriveicon, *nfsdriveumticon;
FXIcon *onepanelicon, *packageicon, *paste_clpicon, *prefsicon, *printbigicon, *printicon;
FXIcon *questionbigicon, *quiticon, *redoicon, *reloadicon, *renameiticon, *replaceicon;
FXIcon *reverticon, *rotatelefticon, *rotaterighticon, *runicon, *saveasicon, *savefileicon;
FXIcon *searchnexticon, *searchicon, *searchprevicon, *selallicon, *setbookicon, *shellicon;
FXIcon *showhiddenicon, *shownumbersicon, *showthumbicon, *smalliconsicon;
FXIcon *trash_full_bigicon, *trash_fullicon, *treeonepanelicon, *treetwopanelsicon, *twopanelsicon;
FXIcon *undoicon, *unmaphosticon, *uppercaseicon, *warningbigicon, *workicon, *wrapofficon, *wraponicon, *xfeicon, *xfiicon;
FXIcon *xfpicon, *xfvicon, *xfwicon, *zipicon, *zoom100icon, *zoominicon, *zoomouticon, *zoomwinicon;
FXIcon *totrashicon, *dirbackicon, *dirforwardicon, *minixfeicon, *minixferooticon, *filedialogicon, *bigarchaddicon;
FXIcon *switchpanelsicon, *syncpanelsicon, *newlinkicon, *greenbuttonicon, *graybuttonicon;
FXIcon *keybindingsicon, *minikeybindingsicon, *filerestoreicon, *restore_bigicon;


// Load a PNG icon from a file in the icon path
FXIcon* loadiconfile(FXApp *app, const FXString iconpath, const FXString iconname)
{
	// New PNG icon
	FXIcon *icon=NULL;
	icon=new FXPNGIcon(app);

	if(icon)
	{
		// Find icon in the icon directory
		FXString iconfile=FXPath::search(iconpath,iconname.text());

		if(!iconfile.empty())
		{
			FXFileStream str;

			// Try open the file
			if(str.open(iconfile,FXStreamLoad))
			{
				// Load it
				icon->loadPixels(str);

				// Create it
				icon->create();

				// Done
				str.close();

				return icon;
			}
		}

		// Failed, delete the icon
		fprintf(stderr,_("Error: Failed to load %s icon. Please check your icon path...\n"),iconname.text());
		delete icon;
	}
    return NULL;
}


// Load all application icons as global variables
FXbool loadAppIcons(FXApp *app)
{
	FXbool success=TRUE;
	
	// Icon path
	FXString iconpath=app->reg().readStringEntry("SETTINGS","iconpath",DEFAULTICONPATH);
		
	// Load icons and set the success flag
	success=( (archaddicon=loadiconfile(app,iconpath,"archadd.png")) != NULL ) & success;
	success=( (archexticon=loadiconfile(app,iconpath,"archext.png")) != NULL ) & success;
	success=( (attribicon=loadiconfile(app,iconpath,"attrib.png")) != NULL ) & success;
	success=( (bigblockdevicon=loadiconfile(app,iconpath,"bigblockdev.png")) != NULL ) & success;
	success=( (bigbrokenlinkicon=loadiconfile(app,iconpath,"bigbrokenlink.png")) != NULL ) & success;
	success=( (bigcdromicon=loadiconfile(app,iconpath,"bigcdrom.png")) != NULL ) & success;
	success=( (bigchardevicon=loadiconfile(app,iconpath,"bigchardev.png")) != NULL ) & success;
	success=( (bigdocicon=loadiconfile(app,iconpath,"bigdoc.png")) != NULL ) & success;
	success=( (bigexecicon=loadiconfile(app,iconpath,"bigexec.png")) != NULL ) & success;
	success=( (bigfloppyicon=loadiconfile(app,iconpath,"bigfloppy.png")) != NULL ) & success;
	success=( (bigfolderlockedicon=loadiconfile(app,iconpath,"bigfolderlocked.png")) != NULL ) & success;
	success=( (bigfolderopenicon=loadiconfile(app,iconpath,"bigfolderopen.png")) != NULL ) & success;
	success=( (bigfoldericon=loadiconfile(app,iconpath,"bigfolder.png")) != NULL ) & success;
	success=( (bigfolderupicon=loadiconfile(app,iconpath,"bigfolderup.png")) != NULL ) & success;
	success=( (bigharddiskicon=loadiconfile(app,iconpath,"bigharddisk.png")) != NULL ) & success;
	success=( (bigiconsicon=loadiconfile(app,iconpath,"bigicons.png")) != NULL ) & success;
	success=( (biglinkicon=loadiconfile(app,iconpath,"biglink.png")) != NULL ) & success;
	success=( (bignfsdriveicon=loadiconfile(app,iconpath,"bignfsdrive.png")) != NULL ) & success;
	success=( (bignfsdriveumticon=loadiconfile(app,iconpath,"bignfsdriveumt.png")) != NULL ) & success;
	success=( (bigpipeicon=loadiconfile(app,iconpath,"bigpipe.png")) != NULL ) & success;
	success=( (bigsocketicon=loadiconfile(app,iconpath,"bigsocket.png")) != NULL ) & success;
	success=( (bigzipicon=loadiconfile(app,iconpath,"bigzip.png")) != NULL ) & success;
	success=( (cdromicon=loadiconfile(app,iconpath,"cdrom.png")) != NULL ) & success;
	success=( (closefileicon=loadiconfile(app,iconpath,"closefile.png")) != NULL ) & success;
	success=( (clrbookicon=loadiconfile(app,iconpath,"clrbook.png")) != NULL ) & success;
	success=( (colltreeicon=loadiconfile(app,iconpath,"colltree.png")) != NULL ) & success;
	success=( (copy_bigicon=loadiconfile(app,iconpath,"copy_big.png")) != NULL ) & success;
	success=( (copy_clpicon=loadiconfile(app,iconpath,"copy_clp.png")) != NULL ) & success;
	success=( (cut_clpicon=loadiconfile(app,iconpath,"cut_clp.png")) != NULL ) & success;
	success=( (delete_big_permicon=loadiconfile(app,iconpath,"delete_big_perm.png")) != NULL ) & success;
	success=( (delete_bigicon=loadiconfile(app,iconpath,"delete_big.png")) != NULL ) & success;
	success=( (deselicon=loadiconfile(app,iconpath,"desel.png")) != NULL ) & success;
	success=( (detailsicon=loadiconfile(app,iconpath,"details.png")) != NULL ) & success;
	success=( (dirupicon=loadiconfile(app,iconpath,"dirup.png")) != NULL ) & success;
	success=( (editicon=loadiconfile(app,iconpath,"edit.png")) != NULL ) & success;
	success=( (entericon=loadiconfile(app,iconpath,"enter.png")) != NULL ) & success;
	success=( (errorbigicon=loadiconfile(app,iconpath,"errorbig.png")) != NULL ) & success;
	success=( (exptreeicon=loadiconfile(app,iconpath,"exptree.png")) != NULL ) & success;
	success=( (filedelete_permicon=loadiconfile(app,iconpath,"filedelete_perm.png")) != NULL ) & success;
	success=( (filedeleteicon=loadiconfile(app,iconpath,"filedelete.png")) != NULL ) & success;
	success=( (fileopenicon=loadiconfile(app,iconpath,"fileopen.png")) != NULL ) & success;
	success=( (viewicon=loadiconfile(app,iconpath,"view.png")) != NULL ) & success;
	success=( (filtericon=loadiconfile(app,iconpath,"filter.png")) != NULL ) & success;
	success=( (find_againicon=loadiconfile(app,iconpath,"find_again.png")) != NULL ) & success;
	success=( (fliplricon=loadiconfile(app,iconpath,"fliplr.png")) != NULL ) & success;
	success=( (flipudicon=loadiconfile(app,iconpath,"flipud.png")) != NULL ) & success;
	success=( (floppyicon=loadiconfile(app,iconpath,"floppy.png")) != NULL ) & success;
	success=( (fontsicon=loadiconfile(app,iconpath,"fonts.png")) != NULL ) & success;
	success=( (gotobigicon=loadiconfile(app,iconpath,"gotobig.png")) != NULL ) & success;
	success=( (gotolineicon=loadiconfile(app,iconpath,"gotoline.png")) != NULL ) & success;
	success=( (harddiskicon=loadiconfile(app,iconpath,"harddisk.png")) != NULL ) & success;
	success=( (helpicon=loadiconfile(app,iconpath,"help.png")) != NULL ) & success;
	success=( (hidehiddenicon=loadiconfile(app,iconpath,"hidehidden.png")) != NULL ) & success;
	success=( (hidenumbersicon=loadiconfile(app,iconpath,"hidenumbers.png")) != NULL ) & success;
	success=( (hidethumbicon=loadiconfile(app,iconpath,"hidethumb.png")) != NULL ) & success;
	success=( (homeicon=loadiconfile(app,iconpath,"home.png")) != NULL ) & success;
	success=( (infobigicon=loadiconfile(app,iconpath,"infobig.png")) != NULL ) & success;
	success=( (invselicon=loadiconfile(app,iconpath,"invsel.png")) != NULL ) & success;
	success=( (link_bigicon=loadiconfile(app,iconpath,"link_big.png")) != NULL ) & success;
	success=( (locationicon=loadiconfile(app,iconpath,"location.png")) != NULL ) & success;
	success=( (lowercaseicon=loadiconfile(app,iconpath,"lowercase.png")) != NULL ) & success;
	success=( (maphosticon=loadiconfile(app,iconpath,"maphost.png")) != NULL ) & success;
	success=( (miniappicon=loadiconfile(app,iconpath,"miniapp.png")) != NULL ) & success;
	success=( (miniblockdevicon=loadiconfile(app,iconpath,"miniblockdev.png")) != NULL ) & success;
	success=( (minibrokenlinkicon=loadiconfile(app,iconpath,"minibrokenlink.png")) != NULL ) & success;
	success=( (minichardevicon=loadiconfile(app,iconpath,"minichardev.png")) != NULL ) & success;
	success=( (minidocicon=loadiconfile(app,iconpath,"minidoc.png")) != NULL ) & success;
	success=( (miniexecicon=loadiconfile(app,iconpath,"miniexec.png")) != NULL ) & success;
	success=( (minifolderclosedicon=loadiconfile(app,iconpath,"minifolderclosed.png")) != NULL ) & success;
	success=( (minifolderlockedicon=loadiconfile(app,iconpath,"minifolderlocked.png")) != NULL ) & success;
	success=( (minifolderopenicon=loadiconfile(app,iconpath,"minifolderopen.png")) != NULL ) & success;
	success=( (minifoldericon=loadiconfile(app,iconpath,"minifolder.png")) != NULL ) & success;
	success=( (minifolderupicon=loadiconfile(app,iconpath,"minifolderup.png")) != NULL ) & success;
	success=( (minilinkicon=loadiconfile(app,iconpath,"minilink.png")) != NULL ) & success;
	success=( (minipipeicon=loadiconfile(app,iconpath,"minipipe.png")) != NULL ) & success;
	success=( (minishellicon=loadiconfile(app,iconpath,"minishell.png")) != NULL ) & success;
	success=( (minisocketicon=loadiconfile(app,iconpath,"minisocket.png")) != NULL ) & success;
	success=( (move_bigicon=loadiconfile(app,iconpath,"move_big.png")) != NULL ) & success;
	success=( (moveiticon=loadiconfile(app,iconpath,"moveit.png")) != NULL ) & success;
	success=( (newfileicon=loadiconfile(app,iconpath,"newfile.png")) != NULL ) & success;
	success=( (newfoldericon=loadiconfile(app,iconpath,"newfolder.png")) != NULL ) & success;
	success=( (nfsdriveicon=loadiconfile(app,iconpath,"nfsdrive.png")) != NULL ) & success;
	success=( (nfsdriveumticon=loadiconfile(app,iconpath,"nfsdriveumt.png")) != NULL ) & success;
	success=( (onepanelicon=loadiconfile(app,iconpath,"onepanel.png")) != NULL ) & success;
	success=( (packageicon=loadiconfile(app,iconpath,"package.png")) != NULL ) & success;
	success=( (paste_clpicon=loadiconfile(app,iconpath,"paste_clp.png")) != NULL ) & success;
	success=( (prefsicon=loadiconfile(app,iconpath,"prefs.png")) != NULL ) & success;
	success=( (printbigicon=loadiconfile(app,iconpath,"printbig.png")) != NULL ) & success;
	success=( (printicon=loadiconfile(app,iconpath,"print.png")) != NULL ) & success;
	success=( (questionbigicon=loadiconfile(app,iconpath,"questionbig.png")) != NULL ) & success;
	success=( (quiticon=loadiconfile(app,iconpath,"quit.png")) != NULL ) & success;
	success=( (redoicon=loadiconfile(app,iconpath,"redo.png")) != NULL ) & success;
	success=( (reloadicon=loadiconfile(app,iconpath,"reload.png")) != NULL ) & success;
	success=( (renameiticon=loadiconfile(app,iconpath,"renameit.png")) != NULL ) & success;
	success=( (replaceicon=loadiconfile(app,iconpath,"replace.png")) != NULL ) & success;
	success=( (reverticon=loadiconfile(app,iconpath,"revert.png")) != NULL ) & success;
	success=( (rotatelefticon=loadiconfile(app,iconpath,"rotateleft.png")) != NULL ) & success;
	success=( (rotaterighticon=loadiconfile(app,iconpath,"rotateright.png")) != NULL ) & success;
	success=( (runicon=loadiconfile(app,iconpath,"run.png")) != NULL ) & success;
	success=( (saveasicon=loadiconfile(app,iconpath,"saveas.png")) != NULL ) & success;
	success=( (savefileicon=loadiconfile(app,iconpath,"savefile.png")) != NULL ) & success;
	success=( (searchnexticon=loadiconfile(app,iconpath,"searchnext.png")) != NULL ) & success;
	success=( (searchicon=loadiconfile(app,iconpath,"search.png")) != NULL ) & success;
	success=( (searchprevicon=loadiconfile(app,iconpath,"searchprev.png")) != NULL ) & success;
	success=( (selallicon=loadiconfile(app,iconpath,"selall.png")) != NULL ) & success;
	success=( (setbookicon=loadiconfile(app,iconpath,"setbook.png")) != NULL ) & success;
	success=( (shellicon=loadiconfile(app,iconpath,"shell.png")) != NULL ) & success;
	success=( (showhiddenicon=loadiconfile(app,iconpath,"showhidden.png")) != NULL ) & success;
	success=( (shownumbersicon=loadiconfile(app,iconpath,"shownumbers.png")) != NULL ) & success;
	success=( (showthumbicon=loadiconfile(app,iconpath,"showthumb.png")) != NULL ) & success;
	success=( (smalliconsicon=loadiconfile(app,iconpath,"smallicons.png")) != NULL ) & success;
	success=( (trash_full_bigicon=loadiconfile(app,iconpath,"trash_full_big.png")) != NULL ) & success;
	success=( (trash_fullicon=loadiconfile(app,iconpath,"trash_full.png")) != NULL ) & success;
	success=( (treeonepanelicon=loadiconfile(app,iconpath,"treeonepanel.png")) != NULL ) & success;
	success=( (treetwopanelsicon=loadiconfile(app,iconpath,"treetwopanels.png")) != NULL ) & success;
	success=( (twopanelsicon=loadiconfile(app,iconpath,"twopanels.png")) != NULL ) & success;
	success=( (undoicon=loadiconfile(app,iconpath,"undo.png")) != NULL ) & success;
	success=( (unmaphosticon=loadiconfile(app,iconpath,"unmaphost.png")) != NULL ) & success;
	success=( (uppercaseicon=loadiconfile(app,iconpath,"uppercase.png")) != NULL ) & success;
	success=( (warningbigicon=loadiconfile(app,iconpath,"warningbig.png")) != NULL ) & success;
	success=( (workicon=loadiconfile(app,iconpath,"work.png")) != NULL ) & success;
	success=( (wrapofficon=loadiconfile(app,iconpath,"wrapoff.png")) != NULL ) & success;
	success=( (wraponicon=loadiconfile(app,iconpath,"wrapon.png")) != NULL ) & success;
	success=( (xfeicon=loadiconfile(app,iconpath,"xfe.png")) != NULL ) & success;
	success=( (xfiicon=loadiconfile(app,iconpath,"xfi.png")) != NULL ) & success;
	success=( (xfpicon=loadiconfile(app,iconpath,"xfp.png")) != NULL ) & success;
	success=( (xfvicon=loadiconfile(app,iconpath,"xfv.png")) != NULL ) & success;
	success=( (xfwicon=loadiconfile(app,iconpath,"xfw.png")) != NULL ) & success;
	success=( (zipicon=loadiconfile(app,iconpath,"zip.png")) != NULL ) & success;
	success=( (zoom100icon=loadiconfile(app,iconpath,"zoom100.png")) != NULL ) & success;
	success=( (zoominicon=loadiconfile(app,iconpath,"zoomin.png")) != NULL ) & success;
	success=( (zoomouticon=loadiconfile(app,iconpath,"zoomout.png")) != NULL ) & success;
	success=( (zoomwinicon=loadiconfile(app,iconpath,"zoomwin.png")) != NULL ) & success;
	success=( (totrashicon=loadiconfile(app,iconpath,"totrash.png")) != NULL ) & success;
	success=( (dirbackicon=loadiconfile(app,iconpath,"dirback.png")) != NULL ) & success;
	success=( (dirforwardicon=loadiconfile(app,iconpath,"dirforward.png")) != NULL ) & success;
	success=( (minixferooticon=loadiconfile(app,iconpath,"minixferoot.png")) != NULL ) & success;
	success=( (minixfeicon=loadiconfile(app,iconpath,"minixfe.png")) != NULL ) & success;
	success=( (filedialogicon=loadiconfile(app,iconpath,"filedialog.png")) != NULL ) & success;
	success=( (bigarchaddicon=loadiconfile(app,iconpath,"bigarchadd.png")) != NULL ) & success;
	success=( (switchpanelsicon=loadiconfile(app,iconpath,"switchpanels.png")) != NULL ) & success;
	success=( (syncpanelsicon=loadiconfile(app,iconpath,"syncpanels.png")) != NULL ) & success;
	success=( (newlinkicon=loadiconfile(app,iconpath,"newlink.png")) != NULL ) & success;
	success=( (greenbuttonicon=loadiconfile(app,iconpath,"greenbutton.png")) != NULL ) & success;
	success=( (graybuttonicon=loadiconfile(app,iconpath,"graybutton.png")) != NULL ) & success;
	success=( (keybindingsicon=loadiconfile(app,iconpath,"keybindings.png")) != NULL ) & success;
	success=( (minikeybindingsicon=loadiconfile(app,iconpath,"minikeybindings.png")) != NULL ) & success;
	success=( (filerestoreicon=loadiconfile(app,iconpath,"filerestore.png")) != NULL ) & success;
	success=( (restore_bigicon=loadiconfile(app,iconpath,"restore_big.png")) != NULL ) & success;
		
	return success;
}
