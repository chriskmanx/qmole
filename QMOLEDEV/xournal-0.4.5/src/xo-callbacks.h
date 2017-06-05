#include <gtk/gtk.h>

void
on_fileNew_activate                    (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_fileNewBackground_activate          (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_fileOpen_activate                   (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_fileSave_activate                   (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_fileSaveAs_activate                 (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_filePrintOptions_activate           (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_filePrint_activate                  (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_filePrintPDF_activate               (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_fileQuit_activate                   (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_editUndo_activate                   (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_editRedo_activate                   (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_editCut_activate                    (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_editCopy_activate                   (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_editPaste_activate                  (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_editDelete_activate                 (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_viewContinuous_activate             (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_viewOnePage_activate                (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_viewZoomIn_activate                 (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_viewZoomOut_activate                (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_viewNormalSize_activate             (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_viewPageWidth_activate              (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_viewFirstPage_activate              (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_viewPreviousPage_activate           (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_viewNextPage_activate               (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_viewLastPage_activate               (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_viewShowLayer_activate              (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_viewHideLayer_activate              (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_journalNewPageBefore_activate       (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_journalNewPageAfter_activate        (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_journalNewPageEnd_activate          (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_journalDeletePage_activate          (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_journalNewLayer_activate            (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_journalDeleteLayer_activate         (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_journalFlatten_activate             (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_journalPaperSize_activate           (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_papercolorWhite_activate            (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_papercolorYellow_activate           (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_papercolorPink_activate             (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_papercolorOrange_activate           (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_papercolorBlue_activate             (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_papercolorGreen_activate            (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_papercolorOther_activate            (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_paperstylePlain_activate            (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_paperstyleLined_activate            (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_paperstyleRuled_activate            (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_paperstyleGraph_activate            (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_journalLoadBackground_activate      (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_journalScreenshot_activate          (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_journalApplyAllPages_activate       (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_toolsPen_activate                   (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_toolsEraser_activate                (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_toolsHighlighter_activate           (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_toolsText_activate                  (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_toolsSelectRegion_activate          (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_toolsSelectRectangle_activate       (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_toolsVerticalSpace_activate         (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_colorBlack_activate                 (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_colorBlue_activate                  (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_colorRed_activate                   (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_colorGreen_activate                 (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_colorGray_activate                  (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_colorLightBlue_activate             (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_colorLightGreen_activate            (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_colorMagenta_activate               (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_colorOrange_activate                (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_colorYellow_activate                (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_colorWhite_activate                 (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_colorOther_activate                 (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_penthicknessVeryFine_activate       (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_penthicknessFine_activate           (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_penthicknessMedium_activate         (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_penthicknessThick_activate          (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_penthicknessVeryThick_activate      (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_eraserFine_activate                 (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_eraserMedium_activate               (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_eraserThick_activate                (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_eraserStandard_activate             (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_eraserWhiteout_activate             (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_eraserDeleteStrokes_activate        (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_highlighterFine_activate            (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_highlighterMedium_activate          (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_highlighterThick_activate           (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_toolsTextFont_activate              (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_toolsDefaultPen_activate            (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_toolsDefaultEraser_activate         (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_toolsDefaultHighlighter_activate    (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_toolsDefaultText_activate           (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_toolsSetAsDefault_activate          (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_toolsRuler_activate                 (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_optionsSavePreferences_activate     (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_helpIndex_activate                  (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_helpAbout_activate                  (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_toolEraser_activate                 (GtkToolButton   *toolbutton,
                                        gpointer         user_data);

void
on_buttonToolDefault_clicked           (GtkToolButton   *toolbutton,
                                        gpointer         user_data);

void
on_buttonFine_clicked                  (GtkToolButton   *toolbutton,
                                        gpointer         user_data);

void
on_buttonMedium_clicked                (GtkToolButton   *toolbutton,
                                        gpointer         user_data);

void
on_buttonThick_clicked                 (GtkToolButton   *toolbutton,
                                        gpointer         user_data);

gboolean
on_canvas_button_press_event           (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data);

gboolean
on_canvas_button_release_event         (GtkWidget       *widget,
                                        GdkEventButton  *event,
                                        gpointer         user_data);

gboolean
on_canvas_enter_notify_event           (GtkWidget       *widget,
                                        GdkEventCrossing *event,
                                        gpointer         user_data);

gboolean
on_canvas_leave_notify_event           (GtkWidget       *widget,
                                        GdkEventCrossing *event,
                                        gpointer         user_data);

gboolean
on_canvas_expose_event                 (GtkWidget       *widget,
                                        GdkEventExpose  *event,
                                        gpointer         user_data);

gboolean
on_canvas_key_press_event              (GtkWidget       *widget,
                                        GdkEventKey     *event,
                                        gpointer         user_data);

gboolean
on_canvas_motion_notify_event          (GtkWidget       *widget,
                                        GdkEventMotion  *event,
                                        gpointer         user_data);

void
on_comboLayer_changed                  (GtkComboBox     *combobox,
                                        gpointer         user_data);

gboolean
on_winMain_delete_event                (GtkWidget       *widget,
                                        GdkEvent        *event,
                                        gpointer         user_data);

void
on_optionsUseXInput_activate           (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_vscroll_changed                     (GtkAdjustment   *adjustment,
                                        gpointer        user_data);

void
on_spinPageNo_value_changed            (GtkSpinButton   *spinbutton,
                                        gpointer         user_data);

void
on_journalDefaultBackground_activate   (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_journalSetAsDefault_activate        (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_comboStdSizes_changed               (GtkComboBox     *combobox,
                                        gpointer         user_data);

void
on_entryWidth_changed                  (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_entryHeight_changed                 (GtkEditable     *editable,
                                        gpointer         user_data);

void
on_comboUnit_changed                   (GtkComboBox     *combobox,
                                        gpointer         user_data);

void
on_viewFullscreen_activate             (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_optionsButtonMappings_activate      (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_optionsProgressiveBG_activate       (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_mru_activate                        (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button2Pen_activate                 (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button2Eraser_activate              (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button2Highlighter_activate         (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button2Text_activate                (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button2SelectRegion_activate        (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button2SelectRectangle_activate     (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button2VerticalSpace_activate       (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button2LinkBrush_activate           (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button2CopyBrush_activate           (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button3Pen_activate                 (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button3Eraser_activate              (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button3Highlighter_activate         (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button3Text_activate                (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button3SelectRegion_activate        (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button3SelectRectangle_activate     (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button3VerticalSpace_activate       (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button3LinkBrush_activate           (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button3CopyBrush_activate           (GtkMenuItem     *menuitem,
                                        gpointer         user_data);


void
on_viewSetZoom_activate                (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_spinZoom_value_changed              (GtkSpinButton   *spinbutton,
                                        gpointer         user_data);

void
on_radioZoom_toggled                   (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
on_radioZoom100_toggled                (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
on_radioZoomWidth_toggled              (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
on_radioZoomHeight_toggled             (GtkToggleButton *togglebutton,
                                        gpointer         user_data);

void
on_toolsHand_activate                  (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button2Hand_activate                (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_button3Hand_activate                (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_optionsPrintRuling_activate         (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_optionsAutoloadPdfXoj_activate      (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_fontButton_font_set                 (GtkFontButton   *fontbutton,
                                        gpointer         user_data);

void
on_optionsLeftHanded_activate          (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_optionsShortenMenus_activate        (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_optionsAutoSavePrefs_activate       (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_toolsReco_activate                  (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_optionsPressureSensitive_activate   (GtkMenuItem     *menuitem,
                                        gpointer         user_data);

void
on_buttonColorChooser_set              (GtkColorButton  *colorbutton,
                                        gpointer         user_data);

void
on_optionsButtonsSwitchMappings_activate(GtkMenuItem    *menuitem,
                                        gpointer         user_data);
