namespace TypeInferred.HashBang

[<ReflectedDefinition>]
type MimeType = 
    | MimeType of string
    member x.Mime = 
        let (MimeType mime) = x
        mime
[<ReflectedDefinition>]
module MimeTypes =
    let lookupByExtension =
        Map [|
            "N/A", MimeType "application/andrew-inset"
            ".aw", MimeType "application/applixware"
            ".atom", MimeType "application/atom+xml"
            ".xml", MimeType "application/atom+xml"
            ".atomcat", MimeType "application/atomcat+xml"
            ".atomsvc", MimeType "application/atomsvc+xml"
            ".ccxml", MimeType "application/ccxml+xml,"
            ".cdmia", MimeType "application/cdmi-capability"
            ".cdmic", MimeType "application/cdmi-container"
            ".cdmid", MimeType "application/cdmi-domain"
            ".cdmio", MimeType "application/cdmi-object"
            ".cdmiq", MimeType "application/cdmi-queue"
            ".cu", MimeType "application/cu-seeme"
            ".davmount", MimeType "application/davmount+xml"
            ".dssc", MimeType "application/dssc+der"
            ".xdssc", MimeType "application/dssc+xml"
            ".es", MimeType "application/ecmascript"
            ".emma", MimeType "application/emma+xml"
            ".epub", MimeType "application/epub+zip"
            ".exi", MimeType "application/exi"
            ".pfr", MimeType "application/font-tdpfr"
            ".stk", MimeType "application/hyperstudio"
            ".ipfix", MimeType "application/ipfix"
            ".jar", MimeType "application/java-archive"
            ".ser", MimeType "application/java-serialized-object"
            ".class", MimeType "application/java-vm"
            ".js", MimeType "application/javascript"
            ".json", MimeType "application/json"
            ".mads", MimeType "application/mads+xml"
            ".mrc", MimeType "application/marc"
            ".mrcx", MimeType "application/marcxml+xml"
            ".ma", MimeType "application/mathematica"
            ".mathml", MimeType "application/mathml+xml"
            ".mbox", MimeType "application/mbox"
            ".mscml", MimeType "application/mediaservercontrol+xml"
            ".meta4", MimeType "application/metalink4+xml"
            ".mets", MimeType "application/mets+xml"
            ".mods", MimeType "application/mods+xml"
            ".m21", MimeType "application/mp21"
            ".mp4", MimeType "application/mp4"
            ".doc", MimeType "application/msword"
            ".mxf", MimeType "application/mxf"
            ".bin", MimeType "application/octet-stream"
            ".oda", MimeType "application/oda"
            ".opf", MimeType "application/oebps-package+xml"
            ".ogx", MimeType "application/ogg"
            ".onetoc", MimeType "application/onenote"
            ".xer", MimeType "application/patch-ops-error+xml"
            ".pdf", MimeType "application/pdf"
            ".pgp", MimeType "application/pgp-signature"
            ".prf", MimeType "application/pics-rules"
            ".p10", MimeType "application/pkcs10"
            ".p7m", MimeType "application/pkcs7-mime"
            ".p7s", MimeType "application/pkcs7-signature"
            ".p8", MimeType "application/pkcs8"
            ".ac", MimeType "application/pkix-attr-cert"
            ".cer", MimeType "application/pkix-cert"
            ".crl", MimeType "application/pkix-crl"
            ".pkipath", MimeType "application/pkix-pkipath"
            ".pki", MimeType "application/pkixcmp"
            ".pls", MimeType "application/pls+xml"
            ".ai", MimeType "application/postscript"
            ".cww", MimeType "application/prs.cww"
            ".pskcxml", MimeType "application/pskc+xml"
            ".rdf", MimeType "application/rdf+xml"
            ".rif", MimeType "application/reginfo+xml"
            ".rnc", MimeType "application/relax-ng-compact-syntax"
            ".rl", MimeType "application/resource-lists+xml"
            ".rld", MimeType "application/resource-lists-diff+xml"
            ".rs", MimeType "application/rls-services+xml"
            ".rsd", MimeType "application/rsd+xml"
            ".rss", MimeType "application/rss+xml"
            ".xml", MimeType "application/rss+xml"
            ".rtf", MimeType "application/rtf"
            ".sbml", MimeType "application/sbml+xml"
            ".scq", MimeType "application/scvp-cv-request"
            ".scs", MimeType "application/scvp-cv-response"
            ".spq", MimeType "application/scvp-vp-request"
            ".spp", MimeType "application/scvp-vp-response"
            ".sdp", MimeType "application/sdp"
            ".setpay", MimeType "application/set-payment-initiation"
            ".setreg", MimeType "application/set-registration-initiation"
            ".shf", MimeType "application/shf+xml"
            ".smi", MimeType "application/smil+xml"
            ".rq", MimeType "application/sparql-query"
            ".srx", MimeType "application/sparql-results+xml"
            ".gram", MimeType "application/srgs"
            ".grxml", MimeType "application/srgs+xml"
            ".sru", MimeType "application/sru+xml"
            ".ssml", MimeType "application/ssml+xml"
            ".tei", MimeType "application/tei+xml"
            ".tfi", MimeType "application/thraud+xml"
            ".tsd", MimeType "application/timestamped-data"
            ".plb", MimeType "application/vnd.3gpp.pic-bw-large"
            ".psb", MimeType "application/vnd.3gpp.pic-bw-small"
            ".pvb", MimeType "application/vnd.3gpp.pic-bw-var"
            ".tcap", MimeType "application/vnd.3gpp2.tcap"
            ".pwn", MimeType "application/vnd.3m.post-it-notes"
            ".aso", MimeType "application/vnd.accpac.simply.aso"
            ".imp", MimeType "application/vnd.accpac.simply.imp"
            ".acu", MimeType "application/vnd.acucobol"
            ".atc", MimeType "application/vnd.acucorp"
            ".air", MimeType "application/vnd.adobe.air-application-installer-package+zip"
            ".fxp", MimeType "application/vnd.adobe.fxp"
            ".xdp", MimeType "application/vnd.adobe.xdp+xml"
            ".xfdf", MimeType "application/vnd.adobe.xfdf"
            ".ahead", MimeType "application/vnd.ahead.space"
            ".azf", MimeType "application/vnd.airzip.filesecure.azf"
            ".azs", MimeType "application/vnd.airzip.filesecure.azs"
            ".azw", MimeType "application/vnd.amazon.ebook"
            ".acc", MimeType "application/vnd.americandynamics.acc"
            ".ami", MimeType "application/vnd.amiga.ami"
            ".apk", MimeType "application/vnd.android.package-archive"
            ".cii", MimeType "application/vnd.anser-web-certificate-issue-initiation"
            ".fti", MimeType "application/vnd.anser-web-funds-transfer-initiation"
            ".atx", MimeType "application/vnd.antix.game-component"
            ".mpkg", MimeType "application/vnd.apple.installer+xml"
            ".m3u8", MimeType "application/vnd.apple.mpegurl"
            ".swi", MimeType "application/vnd.aristanetworks.swi"
            ".aep", MimeType "application/vnd.audiograph"
            ".mpm", MimeType "application/vnd.blueice.multipass"
            ".bmi", MimeType "application/vnd.bmi"
            ".rep", MimeType "application/vnd.businessobjects"
            ".cdxml", MimeType "application/vnd.chemdraw+xml"
            ".mmd", MimeType "application/vnd.chipnuts.karaoke-mmd"
            ".cdy", MimeType "application/vnd.cinderella"
            ".cla", MimeType "application/vnd.claymore"
            ".rp9", MimeType "application/vnd.cloanto.rp9"
            ".c4g", MimeType "application/vnd.clonk.c4group"
            ".c11amc", MimeType "application/vnd.cluetrust.cartomobile-config"
            ".c11amz", MimeType "application/vnd.cluetrust.cartomobile-config-pkg"
            ".csp", MimeType "application/vnd.commonspace"
            ".cdbcmsg", MimeType "application/vnd.contact.cmsg"
            ".cmc", MimeType "application/vnd.cosmocaller"
            ".clkx", MimeType "application/vnd.crick.clicker"
            ".clkk", MimeType "application/vnd.crick.clicker.keyboard"
            ".clkp", MimeType "application/vnd.crick.clicker.palette"
            ".clkt", MimeType "application/vnd.crick.clicker.template"
            ".clkw", MimeType "application/vnd.crick.clicker.wordbank"
            ".wbs", MimeType "application/vnd.criticaltools.wbs+xml"
            ".pml", MimeType "application/vnd.ctc-posml"
            ".ppd", MimeType "application/vnd.cups-ppd"
            ".car", MimeType "application/vnd.curl.car"
            ".pcurl", MimeType "application/vnd.curl.pcurl"
            ".rdz", MimeType "application/vnd.data-vision.rdz"
            ".fe_launch", MimeType "application/vnd.denovo.fcselayout-link"
            ".dna", MimeType "application/vnd.dna"
            ".mlp", MimeType "application/vnd.dolby.mlp"
            ".dpg", MimeType "application/vnd.dpgraph"
            ".dfac", MimeType "application/vnd.dreamfactory"
            ".ait", MimeType "application/vnd.dvb.ait"
            ".svc", MimeType "application/vnd.dvb.service"
            ".geo", MimeType "application/vnd.dynageo"
            ".mag", MimeType "application/vnd.ecowin.chart"
            ".nml", MimeType "application/vnd.enliven"
            ".esf", MimeType "application/vnd.epson.esf"
            ".msf", MimeType "application/vnd.epson.msf"
            ".qam", MimeType "application/vnd.epson.quickanime"
            ".slt", MimeType "application/vnd.epson.salt"
            ".ssf", MimeType "application/vnd.epson.ssf"
            ".es3", MimeType "application/vnd.eszigno3+xml"
            ".ez2", MimeType "application/vnd.ezpix-album"
            ".ez3", MimeType "application/vnd.ezpix-package"
            ".fdf", MimeType "application/vnd.fdf"
            ".seed", MimeType "application/vnd.fdsn.seed"
            ".gph", MimeType "application/vnd.flographit"
            ".ftc", MimeType "application/vnd.fluxtime.clip"
            ".fm", MimeType "application/vnd.framemaker"
            ".fnc", MimeType "application/vnd.frogans.fnc"
            ".ltf", MimeType "application/vnd.frogans.ltf"
            ".fsc", MimeType "application/vnd.fsc.weblaunch"
            ".oas", MimeType "application/vnd.fujitsu.oasys"
            ".oa2", MimeType "application/vnd.fujitsu.oasys2"
            ".oa3", MimeType "application/vnd.fujitsu.oasys3"
            ".fg5", MimeType "application/vnd.fujitsu.oasysgp"
            ".bh2", MimeType "application/vnd.fujitsu.oasysprs"
            ".ddd", MimeType "application/vnd.fujixerox.ddd"
            ".xdw", MimeType "application/vnd.fujixerox.docuworks"
            ".xbd", MimeType "application/vnd.fujixerox.docuworks.binder"
            ".fzs", MimeType "application/vnd.fuzzysheet"
            ".txd", MimeType "application/vnd.genomatix.tuxedo"
            ".ggb", MimeType "application/vnd.geogebra.file"
            ".ggt", MimeType "application/vnd.geogebra.tool"
            ".gex", MimeType "application/vnd.geometry-explorer"
            ".gxt", MimeType "application/vnd.geonext"
            ".g2w", MimeType "application/vnd.geoplan"
            ".g3w", MimeType "application/vnd.geospace"
            ".gmx", MimeType "application/vnd.gmx"
            ".kml", MimeType "application/vnd.google-earth.kml+xml"
            ".kmz", MimeType "application/vnd.google-earth.kmz"
            ".gqf", MimeType "application/vnd.grafeq"
            ".gac", MimeType "application/vnd.groove-account"
            ".ghf", MimeType "application/vnd.groove-help"
            ".gim", MimeType "application/vnd.groove-identity-message"
            ".grv", MimeType "application/vnd.groove-injector"
            ".gtm", MimeType "application/vnd.groove-tool-message"
            ".tpl", MimeType "application/vnd.groove-tool-template"
            ".vcg", MimeType "application/vnd.groove-vcard"
            ".hal", MimeType "application/vnd.hal+xml"
            ".zmm", MimeType "application/vnd.handheld-entertainment+xml"
            ".hbci", MimeType "application/vnd.hbci"
            ".les", MimeType "application/vnd.hhe.lesson-player"
            ".hpgl", MimeType "application/vnd.hp-hpgl"
            ".hpid", MimeType "application/vnd.hp-hpid"
            ".hps", MimeType "application/vnd.hp-hps"
            ".jlt", MimeType "application/vnd.hp-jlyt"
            ".pcl", MimeType "application/vnd.hp-pcl"
            ".pclxl", MimeType "application/vnd.hp-pclxl"
            ".sfd-hdstx", MimeType "application/vnd.hydrostatix.sof-data"
            ".x3d", MimeType "application/vnd.hzn-3d-crossword"
            ".mpy", MimeType "application/vnd.ibm.minipay"
            ".afp", MimeType "application/vnd.ibm.modcap"
            ".irm", MimeType "application/vnd.ibm.rights-management"
            ".sc", MimeType "application/vnd.ibm.secure-container"
            ".icc", MimeType "application/vnd.iccprofile"
            ".igl", MimeType "application/vnd.igloader"
            ".ivp", MimeType "application/vnd.immervision-ivp"
            ".ivu", MimeType "application/vnd.immervision-ivu"
            ".igm", MimeType "application/vnd.insors.igm"
            ".xpw", MimeType "application/vnd.intercon.formnet"
            ".i2g", MimeType "application/vnd.intergeo"
            ".qbo", MimeType "application/vnd.intu.qbo"
            ".qfx", MimeType "application/vnd.intu.qfx"
            ".rcprofile", MimeType "application/vnd.ipunplugged.rcprofile"
            ".irp", MimeType "application/vnd.irepository.package+xml"
            ".xpr", MimeType "application/vnd.is-xpr"
            ".fcs", MimeType "application/vnd.isac.fcs"
            ".jam", MimeType "application/vnd.jam"
            ".rms", MimeType "application/vnd.jcp.javame.midlet-rms"
            ".jisp", MimeType "application/vnd.jisp"
            ".joda", MimeType "application/vnd.joost.joda-archive"
            ".ktz", MimeType "application/vnd.kahootz"
            ".karbon", MimeType "application/vnd.kde.karbon"
            ".chrt", MimeType "application/vnd.kde.kchart"
            ".kfo", MimeType "application/vnd.kde.kformula"
            ".flw", MimeType "application/vnd.kde.kivio"
            ".kon", MimeType "application/vnd.kde.kontour"
            ".kpr", MimeType "application/vnd.kde.kpresenter"
            ".ksp", MimeType "application/vnd.kde.kspread"
            ".kwd", MimeType "application/vnd.kde.kword"
            ".htke", MimeType "application/vnd.kenameaapp"
            ".kia", MimeType "application/vnd.kidspiration"
            ".kne", MimeType "application/vnd.kinar"
            ".skp", MimeType "application/vnd.koan"
            ".sse", MimeType "application/vnd.kodak-descriptor"
            ".lasxml", MimeType "application/vnd.las.las+xml"
            ".lbd", MimeType "application/vnd.llamagraphics.life-balance.desktop"
            ".lbe", MimeType "application/vnd.llamagraphics.life-balance.exchange+xml"
            ".123", MimeType "application/vnd.lotus-1-2-3"
            ".apr", MimeType "application/vnd.lotus-approach"
            ".pre", MimeType "application/vnd.lotus-freelance"
            ".nsf", MimeType "application/vnd.lotus-notes"
            ".org", MimeType "application/vnd.lotus-organizer"
            ".scm", MimeType "application/vnd.lotus-screencam"
            ".lwp", MimeType "application/vnd.lotus-wordpro"
            ".portpkg", MimeType "application/vnd.macports.portpkg"
            ".mcd", MimeType "application/vnd.mcd"
            ".mc1", MimeType "application/vnd.medcalcdata"
            ".cdkey", MimeType "application/vnd.mediastation.cdkey"
            ".mwf", MimeType "application/vnd.mfer"
            ".mfm", MimeType "application/vnd.mfmp"
            ".flo", MimeType "application/vnd.micrografx.flo"
            ".igx", MimeType "application/vnd.micrografx.igx"
            ".mif", MimeType "application/vnd.mif"
            ".daf", MimeType "application/vnd.mobius.daf"
            ".dis", MimeType "application/vnd.mobius.dis"
            ".mbk", MimeType "application/vnd.mobius.mbk"
            ".mqy", MimeType "application/vnd.mobius.mqy"
            ".msl", MimeType "application/vnd.mobius.msl"
            ".plc", MimeType "application/vnd.mobius.plc"
            ".txf", MimeType "application/vnd.mobius.txf"
            ".mpn", MimeType "application/vnd.mophun.application"
            ".mpc", MimeType "application/vnd.mophun.certificate"
            ".xul", MimeType "application/vnd.mozilla.xul+xml"
            ".cil", MimeType "application/vnd.ms-artgalry"
            ".cab", MimeType "application/vnd.ms-cab-compressed"
            ".xls", MimeType "application/vnd.ms-excel"
            ".xlam", MimeType "application/vnd.ms-excel.addin.macroenabled.12"
            ".xlsb", MimeType "application/vnd.ms-excel.sheet.binary.macroenabled.12"
            ".xlsm", MimeType "application/vnd.ms-excel.sheet.macroenabled.12"
            ".xltm", MimeType "application/vnd.ms-excel.template.macroenabled.12"
            ".eot", MimeType "application/vnd.ms-fontobject"
            ".chm", MimeType "application/vnd.ms-htmlhelp"
            ".ims", MimeType "application/vnd.ms-ims"
            ".lrm", MimeType "application/vnd.ms-lrm"
            ".thmx", MimeType "application/vnd.ms-officetheme"
            ".cat", MimeType "application/vnd.ms-pki.seccat"
            ".stl", MimeType "application/vnd.ms-pki.stl"
            ".ppt", MimeType "application/vnd.ms-powerpoint"
            ".ppam", MimeType "application/vnd.ms-powerpoint.addin.macroenabled.12"
            ".pptm", MimeType "application/vnd.ms-powerpoint.presentation.macroenabled.12"
            ".sldm", MimeType "application/vnd.ms-powerpoint.slide.macroenabled.12"
            ".ppsm", MimeType "application/vnd.ms-powerpoint.slideshow.macroenabled.12"
            ".potm", MimeType "application/vnd.ms-powerpoint.template.macroenabled.12"
            ".mpp", MimeType "application/vnd.ms-project"
            ".docm", MimeType "application/vnd.ms-word.document.macroenabled.12"
            ".dotm", MimeType "application/vnd.ms-word.template.macroenabled.12"
            ".wps", MimeType "application/vnd.ms-works"
            ".wpl", MimeType "application/vnd.ms-wpl"
            ".xps", MimeType "application/vnd.ms-xpsdocument"
            ".mseq", MimeType "application/vnd.mseq"
            ".mus", MimeType "application/vnd.musician"
            ".msty", MimeType "application/vnd.muvee.style"
            ".nlu", MimeType "application/vnd.neurolanguage.nlu"
            ".nnd", MimeType "application/vnd.noblenet-directory"
            ".nns", MimeType "application/vnd.noblenet-sealer"
            ".nnw", MimeType "application/vnd.noblenet-web"
            ".ngdat", MimeType "application/vnd.nokia.n-gage.data"
            ".n-gage", MimeType "application/vnd.nokia.n-gage.symbian.install"
            ".rpst", MimeType "application/vnd.nokia.radio-preset"
            ".rpss", MimeType "application/vnd.nokia.radio-presets"
            ".edm", MimeType "application/vnd.novadigm.edm"
            ".edx", MimeType "application/vnd.novadigm.edx"
            ".ext", MimeType "application/vnd.novadigm.ext"
            ".odc", MimeType "application/vnd.oasis.opendocument.chart"
            ".otc", MimeType "application/vnd.oasis.opendocument.chart-template"
            ".odb", MimeType "application/vnd.oasis.opendocument.database"
            ".odf", MimeType "application/vnd.oasis.opendocument.formula"
            ".odft", MimeType "application/vnd.oasis.opendocument.formula-template"
            ".odg", MimeType "application/vnd.oasis.opendocument.graphics"
            ".otg", MimeType "application/vnd.oasis.opendocument.graphics-template"
            ".odi", MimeType "application/vnd.oasis.opendocument.image"
            ".oti", MimeType "application/vnd.oasis.opendocument.image-template"
            ".odp", MimeType "application/vnd.oasis.opendocument.presentation"
            ".otp", MimeType "application/vnd.oasis.opendocument.presentation-template"
            ".ods", MimeType "application/vnd.oasis.opendocument.spreadsheet"
            ".ots", MimeType "application/vnd.oasis.opendocument.spreadsheet-template"
            ".odt", MimeType "application/vnd.oasis.opendocument.text"
            ".odm", MimeType "application/vnd.oasis.opendocument.text-master"
            ".ott", MimeType "application/vnd.oasis.opendocument.text-template"
            ".oth", MimeType "application/vnd.oasis.opendocument.text-web"
            ".xo", MimeType "application/vnd.olpc-sugar"
            ".dd2", MimeType "application/vnd.oma.dd2+xml"
            ".oxt", MimeType "application/vnd.openofficeorg.extension"
            ".pptx", MimeType "application/vnd.openxmlformats-officedocument.presentationml.presentation"
            ".sldx", MimeType "application/vnd.openxmlformats-officedocument.presentationml.slide"
            ".ppsx", MimeType "application/vnd.openxmlformats-officedocument.presentationml.slideshow"
            ".potx", MimeType "application/vnd.openxmlformats-officedocument.presentationml.template"
            ".xlsx", MimeType "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
            ".xltx", MimeType "application/vnd.openxmlformats-officedocument.spreadsheetml.template"
            ".docx", MimeType "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
            ".dotx", MimeType "application/vnd.openxmlformats-officedocument.wordprocessingml.template"
            ".mgp", MimeType "application/vnd.osgeo.mapguide.package"
            ".dp", MimeType "application/vnd.osgi.dp"
            ".pdb", MimeType "application/vnd.palm"
            ".paw", MimeType "application/vnd.pawaafile"
            ".str", MimeType "application/vnd.pg.format"
            ".ei6", MimeType "application/vnd.pg.osasli"
            ".efif", MimeType "application/vnd.picsel"
            ".wg", MimeType "application/vnd.pmi.widget"
            ".plf", MimeType "application/vnd.pocketlearn"
            ".pbd", MimeType "application/vnd.powerbuilder6"
            ".box", MimeType "application/vnd.previewsystems.box"
            ".mgz", MimeType "application/vnd.proteus.magazine"
            ".qps", MimeType "application/vnd.publishare-delta-tree"
            ".ptid", MimeType "application/vnd.pvi.ptid1"
            ".qxd", MimeType "application/vnd.quark.quarkxpress"
            ".bed", MimeType "application/vnd.realvnc.bed"
            ".mxl", MimeType "application/vnd.recordare.musicxml"
            ".musicxml", MimeType "application/vnd.recordare.musicxml+xml"
            ".cryptonote", MimeType "application/vnd.rig.cryptonote"
            ".cod", MimeType "application/vnd.rim.cod"
            ".rm", MimeType "application/vnd.rn-realmedia"
            ".link66", MimeType "application/vnd.route66.link66+xml"
            ".st", MimeType "application/vnd.sailingtracker.track"
            ".see", MimeType "application/vnd.seemail"
            ".sema", MimeType "application/vnd.sema"
            ".semd", MimeType "application/vnd.semd"
            ".semf", MimeType "application/vnd.semf"
            ".ifm", MimeType "application/vnd.shana.informed.formdata"
            ".itp", MimeType "application/vnd.shana.informed.formtemplate"
            ".iif", MimeType "application/vnd.shana.informed.interchange"
            ".ipk", MimeType "application/vnd.shana.informed.package"
            ".twd", MimeType "application/vnd.simtech-mindmapper"
            ".mmf", MimeType "application/vnd.smaf"
            ".teacher", MimeType "application/vnd.smart.teacher"
            ".sdkm", MimeType "application/vnd.solent.sdkm+xml"
            ".dxp", MimeType "application/vnd.spotfire.dxp"
            ".sfs", MimeType "application/vnd.spotfire.sfs"
            ".sdc", MimeType "application/vnd.stardivision.calc"
            ".sda", MimeType "application/vnd.stardivision.draw"
            ".sdd", MimeType "application/vnd.stardivision.impress"
            ".smf", MimeType "application/vnd.stardivision.math"
            ".sdw", MimeType "application/vnd.stardivision.writer"
            ".sgl", MimeType "application/vnd.stardivision.writer-global"
            ".sm", MimeType "application/vnd.stepmania.stepchart"
            ".sxc", MimeType "application/vnd.sun.xml.calc"
            ".stc", MimeType "application/vnd.sun.xml.calc.template"
            ".sxd", MimeType "application/vnd.sun.xml.draw"
            ".std", MimeType "application/vnd.sun.xml.draw.template"
            ".sxi", MimeType "application/vnd.sun.xml.impress"
            ".sti", MimeType "application/vnd.sun.xml.impress.template"
            ".sxm", MimeType "application/vnd.sun.xml.math"
            ".sxw", MimeType "application/vnd.sun.xml.writer"
            ".sxg", MimeType "application/vnd.sun.xml.writer.global"
            ".stw", MimeType "application/vnd.sun.xml.writer.template"
            ".sus", MimeType "application/vnd.sus-calendar"
            ".svd", MimeType "application/vnd.svd"
            ".sis", MimeType "application/vnd.symbian.install"
            ".xsm", MimeType "application/vnd.syncml+xml"
            ".bdm", MimeType "application/vnd.syncml.dm+wbxml"
            ".xdm", MimeType "application/vnd.syncml.dm+xml"
            ".tao", MimeType "application/vnd.tao.intent-module-archive"
            ".tmo", MimeType "application/vnd.tmobile-livetv"
            ".tpt", MimeType "application/vnd.trid.tpt"
            ".mxs", MimeType "application/vnd.triscape.mxs"
            ".tra", MimeType "application/vnd.trueapp"
            ".ufd", MimeType "application/vnd.ufdl"
            ".utz", MimeType "application/vnd.uiq.theme"
            ".umj", MimeType "application/vnd.umajin"
            ".unityweb", MimeType "application/vnd.unity"
            ".uoml", MimeType "application/vnd.uoml+xml"
            ".vcx", MimeType "application/vnd.vcx"
            ".vsd", MimeType "application/vnd.visio"
            ".vis", MimeType "application/vnd.visionary"
            ".vsf", MimeType "application/vnd.vsf"
            ".wbxml", MimeType "application/vnd.wap.wbxml"
            ".wmlc", MimeType "application/vnd.wap.wmlc"
            ".wmlsc", MimeType "application/vnd.wap.wmlscriptc"
            ".wtb", MimeType "application/vnd.webturbo"
            ".nbp", MimeType "application/vnd.wolfram.player"
            ".wpd", MimeType "application/vnd.wordperfect"
            ".wqd", MimeType "application/vnd.wqd"
            ".stf", MimeType "application/vnd.wt.stf"
            ".xar", MimeType "application/vnd.xara"
            ".xfdl", MimeType "application/vnd.xfdl"
            ".hvd", MimeType "application/vnd.yamaha.hv-dic"
            ".hvs", MimeType "application/vnd.yamaha.hv-script"
            ".hvp", MimeType "application/vnd.yamaha.hv-voice"
            ".osf", MimeType "application/vnd.yamaha.openscoreformat"
            ".osfpvg", MimeType "application/vnd.yamaha.openscoreformat.osfpvg+xml"
            ".saf", MimeType "application/vnd.yamaha.smaf-audio"
            ".spf", MimeType "application/vnd.yamaha.smaf-phrase"
            ".cmp", MimeType "application/vnd.yellowriver-custom-menu"
            ".zir", MimeType "application/vnd.zul"
            ".zaz", MimeType "application/vnd.zzazz.deck+xml"
            ".vxml", MimeType "application/voicexml+xml"
            ".wgt", MimeType "application/widget"
            ".hlp", MimeType "application/winhlp"
            ".wsdl", MimeType "application/wsdl+xml"
            ".wspolicy", MimeType "application/wspolicy+xml"
            ".7z", MimeType "application/x-7z-compressed"
            ".abw", MimeType "application/x-abiword"
            ".ace", MimeType "application/x-ace-compressed"
            ".aab", MimeType "application/x-authorware-bin"
            ".aam", MimeType "application/x-authorware-map"
            ".aas", MimeType "application/x-authorware-seg"
            ".bcpio", MimeType "application/x-bcpio"
            ".torrent", MimeType "application/x-bittorrent"
            ".bz", MimeType "application/x-bzip"
            ".bz2", MimeType "application/x-bzip2"
            ".vcd", MimeType "application/x-cdlink"
            ".chat", MimeType "application/x-chat"
            ".pgn", MimeType "application/x-chess-pgn"
            ".cpio", MimeType "application/x-cpio"
            ".csh", MimeType "application/x-csh"
            ".deb", MimeType "application/x-debian-package"
            ".dir", MimeType "application/x-director"
            ".wad", MimeType "application/x-doom"
            ".ncx", MimeType "application/x-dtbncx+xml"
            ".dtb", MimeType "application/x-dtbook+xml"
            ".res", MimeType "application/x-dtbresource+xml"
            ".dvi", MimeType "application/x-dvi"
            ".bdf", MimeType "application/x-font-bdf"
            ".gsf", MimeType "application/x-font-ghostscript"
            ".psf", MimeType "application/x-font-linux-psf"
            ".otf", MimeType "application/x-font-otf"
            ".pcf", MimeType "application/x-font-pcf"
            ".snf", MimeType "application/x-font-snf"
            ".ttf", MimeType "application/x-font-ttf"
            ".pfa", MimeType "application/x-font-type1"
            ".woff", MimeType "application/x-font-woff"
            ".spl", MimeType "application/x-futuresplash"
            ".gnumeric", MimeType "application/x-gnumeric"
            ".gtar", MimeType "application/x-gtar"
            ".hdf", MimeType "application/x-hdf"
            ".jnlp", MimeType "application/x-java-jnlp-file"
            ".latex", MimeType "application/x-latex"
            ".prc", MimeType "application/x-mobipocket-ebook"
            ".application", MimeType "application/x-ms-application"
            ".wmd", MimeType "application/x-ms-wmd"
            ".wmz", MimeType "application/x-ms-wmz"
            ".xbap", MimeType "application/x-ms-xbap"
            ".mdb", MimeType "application/x-msaccess"
            ".obd", MimeType "application/x-msbinder"
            ".crd", MimeType "application/x-mscardfile"
            ".clp", MimeType "application/x-msclip"
            ".exe", MimeType "application/x-msdownload"
            ".mvb", MimeType "application/x-msmediaview"
            ".wmf", MimeType "application/x-msmetafile"
            ".mny", MimeType "application/x-msmoney"
            ".pub", MimeType "application/x-mspublisher"
            ".scd", MimeType "application/x-msschedule"
            ".trm", MimeType "application/x-msterminal"
            ".wri", MimeType "application/x-mswrite"
            ".nc", MimeType "application/x-netcdf"
            ".p12", MimeType "application/x-pkcs12"
            ".p7b", MimeType "application/x-pkcs7-certificates"
            ".p7r", MimeType "application/x-pkcs7-certreqresp"
            ".rar", MimeType "application/x-rar-compressed"
            ".sh", MimeType "application/x-sh"
            ".shar", MimeType "application/x-shar"
            ".swf", MimeType "application/x-shockwave-flash"
            ".xap", MimeType "application/x-silverlight-app"
            ".sit", MimeType "application/x-stuffit"
            ".sitx", MimeType "application/x-stuffitx"
            ".sv4cpio", MimeType "application/x-sv4cpio"
            ".sv4crc", MimeType "application/x-sv4crc"
            ".tar", MimeType "application/x-tar"
            ".tcl", MimeType "application/x-tcl"
            ".tex", MimeType "application/x-tex"
            ".tfm", MimeType "application/x-tex-tfm"
            ".texinfo", MimeType "application/x-texinfo"
            ".ustar", MimeType "application/x-ustar"
            ".src", MimeType "application/x-wais-source"
            ".der", MimeType "application/x-x509-ca-cert"
            ".fig", MimeType "application/x-xfig"
            ".xpi", MimeType "application/x-xpinstall"
            ".xdf", MimeType "application/xcap-diff+xml"
            ".xenc", MimeType "application/xenc+xml"
            ".xhtml", MimeType "application/xhtml+xml"
            ".xml", MimeType "application/xml"
            ".dtd", MimeType "application/xml-dtd"
            ".xop", MimeType "application/xop+xml"
            ".xslt", MimeType "application/xslt+xml"
            ".xspf", MimeType "application/xspf+xml"
            ".mxml", MimeType "application/xv+xml"
            ".yang", MimeType "application/yang"
            ".yin", MimeType "application/yin+xml"
            ".zip", MimeType "application/zip"
            ".adp", MimeType "audio/adpcm"
            ".au", MimeType "audio/basic"
            ".mid", MimeType "audio/midi"
            ".mp4a", MimeType "audio/mp4"
            ".mpga", MimeType "audio/mpeg"
            ".oga", MimeType "audio/ogg"
            ".uva", MimeType "audio/vnd.dece.audio"
            ".eol", MimeType "audio/vnd.digital-winds"
            ".dra", MimeType "audio/vnd.dra"
            ".dts", MimeType "audio/vnd.dts"
            ".dtshd", MimeType "audio/vnd.dts.hd"
            ".lvp", MimeType "audio/vnd.lucent.voice"
            ".pya", MimeType "audio/vnd.ms-playready.media.pya"
            ".ecelp4800", MimeType "audio/vnd.nuera.ecelp4800"
            ".ecelp7470", MimeType "audio/vnd.nuera.ecelp7470"
            ".ecelp9600", MimeType "audio/vnd.nuera.ecelp9600"
            ".rip", MimeType "audio/vnd.rip"
            ".weba", MimeType "audio/webm"
            ".aac", MimeType "audio/x-aac"
            ".aif", MimeType "audio/x-aiff"
            ".m3u", MimeType "audio/x-mpegurl"
            ".wax", MimeType "audio/x-ms-wax"
            ".wma", MimeType "audio/x-ms-wma"
            ".ram", MimeType "audio/x-pn-realaudio"
            ".rmp", MimeType "audio/x-pn-realaudio-plugin"
            ".wav", MimeType "audio/x-wav"
            ".cdx", MimeType "chemical/x-cdx"
            ".cif", MimeType "chemical/x-cif"
            ".cmdf", MimeType "chemical/x-cmdf"
            ".cml", MimeType "chemical/x-cml"
            ".csml", MimeType "chemical/x-csml"
            ".xyz", MimeType "chemical/x-xyz"
            ".bmp", MimeType "image/bmp"
            ".cgm", MimeType "image/cgm"
            ".g3", MimeType "image/g3fax"
            ".gif", MimeType "image/gif"
            ".ief", MimeType "image/ief"
            ".jpeg", MimeType "image/jpeg"
            ".jpg", MimeType "image/jpeg"
            ".ktx", MimeType "image/ktx"
            ".png", MimeType "image/png"
            ".btif", MimeType "image/prs.btif"
            ".svg", MimeType "image/svg+xml"
            ".tiff", MimeType "image/tiff"
            ".psd", MimeType "image/vnd.adobe.photoshop"
            ".uvi", MimeType "image/vnd.dece.graphic"
            ".sub", MimeType "image/vnd.dvb.subtitle"
            ".djvu", MimeType "image/vnd.djvu"
            ".dwg", MimeType "image/vnd.dwg"
            ".dxf", MimeType "image/vnd.dxf"
            ".fbs", MimeType "image/vnd.fastbidsheet"
            ".fpx", MimeType "image/vnd.fpx"
            ".fst", MimeType "image/vnd.fst"
            ".mmr", MimeType "image/vnd.fujixerox.edmics-mmr"
            ".rlc", MimeType "image/vnd.fujixerox.edmics-rlc"
            ".mdi", MimeType "image/vnd.ms-modi"
            ".npx", MimeType "image/vnd.net-fpx"
            ".wbmp", MimeType "image/vnd.wap.wbmp"
            ".xif", MimeType "image/vnd.xiff"
            ".webp", MimeType "image/webp"
            ".ras", MimeType "image/x-cmu-raster"
            ".cmx", MimeType "image/x-cmx"
            ".fh", MimeType "image/x-freehand"
            ".ico", MimeType "image/x-icon"
            ".pcx", MimeType "image/x-pcx"
            ".pic", MimeType "image/x-pict"
            ".pnm", MimeType "image/x-portable-anymap"
            ".pbm", MimeType "image/x-portable-bitmap"
            ".pgm", MimeType "image/x-portable-graymap"
            ".ppm", MimeType "image/x-portable-pixmap"
            ".rgb", MimeType "image/x-rgb"
            ".xbm", MimeType "image/x-xbitmap"
            ".xpm", MimeType "image/x-xpixmap"
            ".xwd", MimeType "image/x-xwindowdump"
            ".eml", MimeType "message/rfc822"
            ".igs", MimeType "model/iges"
            ".msh", MimeType "model/mesh"
            ".dae", MimeType "model/vnd.collada+xml"
            ".dwf", MimeType "model/vnd.dwf"
            ".gdl", MimeType "model/vnd.gdl"
            ".gtw", MimeType "model/vnd.gtw"
            ".mts", MimeType "model/vnd.mts"
            ".vtu", MimeType "model/vnd.vtu"
            ".wrl", MimeType "model/vrml"
            ".ics", MimeType "text/calendar"
            ".css", MimeType "text/css"
            ".csv", MimeType "text/csv"
            ".html", MimeType "text/html"
            ".n3", MimeType "text/n3"
            ".txt", MimeType "text/plain"
            ".dsc", MimeType "text/prs.lines.tag"
            ".rtx", MimeType "text/richtext"
            ".sgml", MimeType "text/sgml"
            ".tsv", MimeType "text/tab-separated-values"
            ".t", MimeType "text/troff"
            ".ttl", MimeType "text/turtle"
            ".uri", MimeType "text/uri-list"
            ".curl", MimeType "text/vnd.curl"
            ".dcurl", MimeType "text/vnd.curl.dcurl"
            ".scurl", MimeType "text/vnd.curl.scurl"
            ".mcurl", MimeType "text/vnd.curl.mcurl"
            ".fly", MimeType "text/vnd.fly"
            ".flx", MimeType "text/vnd.fmi.flexstor"
            ".gv", MimeType "text/vnd.graphviz"
            ".3dml", MimeType "text/vnd.in3d.3dml"
            ".spot", MimeType "text/vnd.in3d.spot"
            ".jad", MimeType "text/vnd.sun.j2me.app-descriptor"
            ".wml", MimeType "text/vnd.wap.wml"
            ".wmls", MimeType "text/vnd.wap.wmlscript"
            ".s", MimeType "text/x-asm"
            ".c", MimeType "text/x-c"
            ".f", MimeType "text/x-fortran"
            ".p", MimeType "text/x-pascal"
            ".java", MimeType "text/x-java-source,java"
            ".etx", MimeType "text/x-setext"
            ".uu", MimeType "text/x-uuencode"
            ".vcs", MimeType "text/x-vcalendar"
            ".vcf", MimeType "text/x-vcard"
            ".3gp", MimeType "video/3gpp"
            ".3g2", MimeType "video/3gpp2"
            ".h261", MimeType "video/h261"
            ".h263", MimeType "video/h263"
            ".h264", MimeType "video/h264"
            ".jpgv", MimeType "video/jpeg"
            ".jpm", MimeType "video/jpm"
            ".mj2", MimeType "video/mj2"
            ".mp4", MimeType "video/mp4"
            ".mpeg", MimeType "video/mpeg"
            ".ogv", MimeType "video/ogg"
            ".qt", MimeType "video/quicktime"
            ".uvh", MimeType "video/vnd.dece.hd"
            ".uvm", MimeType "video/vnd.dece.mobile"
            ".uvp", MimeType "video/vnd.dece.pd"
            ".uvs", MimeType "video/vnd.dece.sd"
            ".uvv", MimeType "video/vnd.dece.video"
            ".fvt", MimeType "video/vnd.fvt"
            ".mxu", MimeType "video/vnd.mpegurl"
            ".pyv", MimeType "video/vnd.ms-playready.media.pyv"
            ".uvu", MimeType "video/vnd.uvvu.mp4"
            ".viv", MimeType "video/vnd.vivo"
            ".webm", MimeType "video/webm"
            ".f4v", MimeType "video/x-f4v"
            ".fli", MimeType "video/x-fli"
            ".flv", MimeType "video/x-flv"
            ".m4v", MimeType "video/x-m4v"
            ".asf", MimeType "video/x-ms-asf"
            ".wm", MimeType "video/x-ms-wm"
            ".wmv", MimeType "video/x-ms-wmv"
            ".wmx", MimeType "video/x-ms-wmx"
            ".wvx", MimeType "video/x-ms-wvx"
            ".avi", MimeType "video/x-msvideo"
            ".movie", MimeType "video/x-sgi-movie"
            ".ice", MimeType "x-conference/x-cooltalk"
            ".par", MimeType "text/plain-bas"
            ".yaml", MimeType "text/yaml"
        |]

    let fromExtension ext = lookupByExtension.TryFind ext

    [<ReflectedDefinition>]
    module Application =
        /// "application/andrew-inset": Andrew Toolkit
        let andrew_inset = MimeType "application/andrew-inset"

        /// "application/applixware": Applixware
        let applixware = MimeType "application/applixware"

        /// "application/atom+xml": Atom Syndication Format
        let atom_xml = MimeType "application/atom+xml"

        /// "application/atomcat+xml": Atom Publishing Protocol
        let atomcat_xml = MimeType "application/atomcat+xml"

        /// "application/atomsvc+xml": Atom Publishing Protocol Service Document
        let atomsvc_xml = MimeType "application/atomsvc+xml"

        /// "application/ccxml+xml,": Voice Browser Call Control
        let ccxml_xml_ = MimeType "application/ccxml+xml,"

        /// "application/cdmi-capability": Cloud Data Management Interface (CDMI) - Capability
        let cdmi_capability = MimeType "application/cdmi-capability"

        /// "application/cdmi-container": Cloud Data Management Interface (CDMI) - Contaimer
        let cdmi_container = MimeType "application/cdmi-container"

        /// "application/cdmi-domain": Cloud Data Management Interface (CDMI) - Domain
        let cdmi_domain = MimeType "application/cdmi-domain"

        /// "application/cdmi-object": Cloud Data Management Interface (CDMI) - Object
        let cdmi_object = MimeType "application/cdmi-object"

        /// "application/cdmi-queue": Cloud Data Management Interface (CDMI) - Queue
        let cdmi_queue = MimeType "application/cdmi-queue"

        /// "application/cu-seeme": CU-SeeMe
        let cu_seeme = MimeType "application/cu-seeme"

        /// "application/davmount+xml": Web Distributed Authoring and Versioning
        let davmount_xml = MimeType "application/davmount+xml"

        /// "application/dssc+der": Data Structure for the Security Suitability of Cryptographic Algorithms
        let dssc_der = MimeType "application/dssc+der"

        /// "application/dssc+xml": Data Structure for the Security Suitability of Cryptographic Algorithms
        let dssc_xml = MimeType "application/dssc+xml"

        /// "application/ecmascript": ECMAScript
        let ecmascript = MimeType "application/ecmascript"

        /// "application/emma+xml": Extensible MultiModal Annotation
        let emma_xml = MimeType "application/emma+xml"

        /// "application/epub+zip": Electronic Publication
        let epub_zip = MimeType "application/epub+zip"

        /// "application/exi": Efficient XML Interchange
        let exi = MimeType "application/exi"

        /// "application/font-tdpfr": Portable Font Resource
        let font_tdpfr = MimeType "application/font-tdpfr"

        /// "application/hyperstudio": Hyperstudio
        let hyperstudio = MimeType "application/hyperstudio"

        /// "application/ipfix": Internet Protocol Flow Information Export
        let ipfix = MimeType "application/ipfix"

        /// "application/java-archive": Java Archive
        let java_archive = MimeType "application/java-archive"

        /// "application/java-serialized-object": Java Serialized Object
        let java_serialized_object = MimeType "application/java-serialized-object"

        /// "application/java-vm": Java Bytecode File
        let java_vm = MimeType "application/java-vm"

        /// "application/javascript": JavaScript
        let javascript = MimeType "application/javascript"

        /// "application/json": JavaScript Object Notation (JSON)
        let json = MimeType "application/json"

        /// "application/mads+xml": Metadata Authority  Description Schema
        let mads_xml = MimeType "application/mads+xml"

        /// "application/marc": MARC Formats
        let marc = MimeType "application/marc"

        /// "application/marcxml+xml": MARC21 XML Schema
        let marcxml_xml = MimeType "application/marcxml+xml"

        /// "application/mathematica": Mathematica Notebooks
        let mathematica = MimeType "application/mathematica"

        /// "application/mathml+xml": Mathematical Markup Language
        let mathml_xml = MimeType "application/mathml+xml"

        /// "application/mbox": Mbox database files
        let mbox = MimeType "application/mbox"

        /// "application/mediaservercontrol+xml": Media Server Control Markup Language
        let mediaservercontrol_xml = MimeType "application/mediaservercontrol+xml"

        /// "application/metalink4+xml": Metalink
        let metalink4_xml = MimeType "application/metalink4+xml"

        /// "application/mets+xml": Metadata Encoding and Transmission Standard
        let mets_xml = MimeType "application/mets+xml"

        /// "application/mods+xml": Metadata Object Description Schema
        let mods_xml = MimeType "application/mods+xml"

        /// "application/mp21": MPEG-21
        let mp21 = MimeType "application/mp21"

        /// "application/mp4": MPEG4
        let mp4 = MimeType "application/mp4"

        /// "application/msword": Microsoft Word
        let msword = MimeType "application/msword"

        /// "application/mxf": Material Exchange Format
        let mxf = MimeType "application/mxf"

        /// "application/octet-stream": Binary Data
        let octet_stream = MimeType "application/octet-stream"

        /// "application/oda": Office Document Architecture
        let oda = MimeType "application/oda"

        /// "application/oebps-package+xml": Open eBook Publication Structure
        let oebps_package_xml = MimeType "application/oebps-package+xml"

        /// "application/ogg": Ogg
        let ogg = MimeType "application/ogg"

        /// "application/onenote": Microsoft OneNote
        let onenote = MimeType "application/onenote"

        /// "application/patch-ops-error+xml": XML Patch Framework
        let patch_ops_error_xml = MimeType "application/patch-ops-error+xml"

        /// "application/pdf": Adobe Portable Document Format
        let pdf = MimeType "application/pdf"

        /// "application/pgp-signature": Pretty Good Privacy - Signature
        let pgp_signature = MimeType "application/pgp-signature"

        /// "application/pics-rules": PICSRules
        let pics_rules = MimeType "application/pics-rules"

        /// "application/pkcs10": PKCS #10 - Certification Request Standard
        let pkcs10 = MimeType "application/pkcs10"

        /// "application/pkcs7-mime": PKCS #7 - Cryptographic Message Syntax Standard
        let pkcs7_mime = MimeType "application/pkcs7-mime"

        /// "application/pkcs7-signature": PKCS #7 - Cryptographic Message Syntax Standard
        let pkcs7_signature = MimeType "application/pkcs7-signature"

        /// "application/pkcs8": PKCS #8 - Private-Key Information Syntax Standard
        let pkcs8 = MimeType "application/pkcs8"

        /// "application/pkix-attr-cert": Attribute Certificate
        let pkix_attr_cert = MimeType "application/pkix-attr-cert"

        /// "application/pkix-cert": Internet Public Key Infrastructure - Certificate
        let pkix_cert = MimeType "application/pkix-cert"

        /// "application/pkix-crl": Internet Public Key Infrastructure - Certificate Revocation Lists
        let pkix_crl = MimeType "application/pkix-crl"

        /// "application/pkix-pkipath": Internet Public Key Infrastructure - Certification Path
        let pkix_pkipath = MimeType "application/pkix-pkipath"

        /// "application/pkixcmp": Internet Public Key Infrastructure - Certificate Management Protocole
        let pkixcmp = MimeType "application/pkixcmp"

        /// "application/pls+xml": Pronunciation Lexicon Specification
        let pls_xml = MimeType "application/pls+xml"

        /// "application/postscript": PostScript
        let postscript = MimeType "application/postscript"

        /// "application/prs.cww": CU-Writer
        let prs_cww = MimeType "application/prs.cww"

        /// "application/pskc+xml": Portable Symmetric Key Container
        let pskc_xml = MimeType "application/pskc+xml"

        /// "application/rdf+xml": Resource Description Framework
        let rdf_xml = MimeType "application/rdf+xml"

        /// "application/reginfo+xml": IMS Networks
        let reginfo_xml = MimeType "application/reginfo+xml"

        /// "application/relax-ng-compact-syntax": Relax NG Compact Syntax
        let relax_ng_compact_syntax = MimeType "application/relax-ng-compact-syntax"

        /// "application/resource-lists+xml": XML Resource Lists
        let resource_lists_xml = MimeType "application/resource-lists+xml"

        /// "application/resource-lists-diff+xml": XML Resource Lists Diff
        let resource_lists_diff_xml = MimeType "application/resource-lists-diff+xml"

        /// "application/rls-services+xml": XML Resource Lists
        let rls_services_xml = MimeType "application/rls-services+xml"

        /// "application/rsd+xml": Really Simple Discovery
        let rsd_xml = MimeType "application/rsd+xml"

        /// "application/rss+xml": RSS - Really Simple Syndication
        let rss_xml = MimeType "application/rss+xml"

        /// "application/rtf": Rich Text Format
        let rtf = MimeType "application/rtf"

        /// "application/sbml+xml": Systems Biology Markup Language
        let sbml_xml = MimeType "application/sbml+xml"

        /// "application/scvp-cv-request": Server-Based Certificate Validation Protocol - Validation Request
        let scvp_cv_request = MimeType "application/scvp-cv-request"

        /// "application/scvp-cv-response": Server-Based Certificate Validation Protocol - Validation Response
        let scvp_cv_response = MimeType "application/scvp-cv-response"

        /// "application/scvp-vp-request": Server-Based Certificate Validation Protocol - Validation Policies - Request
        let scvp_vp_request = MimeType "application/scvp-vp-request"

        /// "application/scvp-vp-response": Server-Based Certificate Validation Protocol - Validation Policies - Response
        let scvp_vp_response = MimeType "application/scvp-vp-response"

        /// "application/sdp": Session Description Protocol
        let sdp = MimeType "application/sdp"

        /// "application/set-payment-initiation": Secure Electronic Transaction - Payment
        let set_payment_initiation = MimeType "application/set-payment-initiation"

        /// "application/set-registration-initiation": Secure Electronic Transaction - Registration
        let set_registration_initiation = MimeType "application/set-registration-initiation"

        /// "application/shf+xml": S Hexdump Format
        let shf_xml = MimeType "application/shf+xml"

        /// "application/smil+xml": Synchronized Multimedia Integration Language
        let smil_xml = MimeType "application/smil+xml"

        /// "application/sparql-query": SPARQL - Query
        let sparql_query = MimeType "application/sparql-query"

        /// "application/sparql-results+xml": SPARQL - Results
        let sparql_results_xml = MimeType "application/sparql-results+xml"

        /// "application/srgs": Speech Recognition Grammar Specification
        let srgs = MimeType "application/srgs"

        /// "application/srgs+xml": Speech Recognition Grammar Specification - XML
        let srgs_xml = MimeType "application/srgs+xml"

        /// "application/sru+xml": Search/Retrieve via URL Response Format
        let sru_xml = MimeType "application/sru+xml"

        /// "application/ssml+xml": Speech Synthesis Markup Language
        let ssml_xml = MimeType "application/ssml+xml"

        /// "application/tei+xml": Text Encoding and Interchange
        let tei_xml = MimeType "application/tei+xml"

        /// "application/thraud+xml": Sharing Transaction Fraud Data
        let thraud_xml = MimeType "application/thraud+xml"

        /// "application/timestamped-data": Time Stamped Data Envelope
        let timestamped_data = MimeType "application/timestamped-data"

        /// "application/vnd.3gpp.pic-bw-large": 3rd Generation Partnership Project - Pic Large
        let vnd_3gpp_pic_bw_large = MimeType "application/vnd.3gpp.pic-bw-large"

        /// "application/vnd.3gpp.pic-bw-small": 3rd Generation Partnership Project - Pic Small
        let vnd_3gpp_pic_bw_small = MimeType "application/vnd.3gpp.pic-bw-small"

        /// "application/vnd.3gpp.pic-bw-var": 3rd Generation Partnership Project - Pic Var
        let vnd_3gpp_pic_bw_var = MimeType "application/vnd.3gpp.pic-bw-var"

        /// "application/vnd.3gpp2.tcap": 3rd Generation Partnership Project - Transaction Capabilities Application Part
        let vnd_3gpp2_tcap = MimeType "application/vnd.3gpp2.tcap"

        /// "application/vnd.3m.post-it-notes": 3M Post It Notes
        let vnd_3m_post_it_notes = MimeType "application/vnd.3m.post-it-notes"

        /// "application/vnd.accpac.simply.aso": Simply Accounting
        let vnd_accpac_simply_aso = MimeType "application/vnd.accpac.simply.aso"

        /// "application/vnd.accpac.simply.imp": Simply Accounting - Data Import
        let vnd_accpac_simply_imp = MimeType "application/vnd.accpac.simply.imp"

        /// "application/vnd.acucobol": ACU Cobol
        let vnd_acucobol = MimeType "application/vnd.acucobol"

        /// "application/vnd.acucorp": ACU Cobol
        let vnd_acucorp = MimeType "application/vnd.acucorp"

        /// "application/vnd.adobe.air-application-installer-package+zip": Adobe AIR Application
        let vnd_adobe_air_application_installer_package_zip = MimeType "application/vnd.adobe.air-application-installer-package+zip"

        /// "application/vnd.adobe.fxp": Adobe Flex Project
        let vnd_adobe_fxp = MimeType "application/vnd.adobe.fxp"

        /// "application/vnd.adobe.xdp+xml": Adobe XML Data Package
        let vnd_adobe_xdp_xml = MimeType "application/vnd.adobe.xdp+xml"

        /// "application/vnd.adobe.xfdf": Adobe XML Forms Data Format
        let vnd_adobe_xfdf = MimeType "application/vnd.adobe.xfdf"

        /// "application/vnd.ahead.space": Ahead AIR Application
        let vnd_ahead_space = MimeType "application/vnd.ahead.space"

        /// "application/vnd.airzip.filesecure.azf": AirZip FileSECURE
        let vnd_airzip_filesecure_azf = MimeType "application/vnd.airzip.filesecure.azf"

        /// "application/vnd.airzip.filesecure.azs": AirZip FileSECURE
        let vnd_airzip_filesecure_azs = MimeType "application/vnd.airzip.filesecure.azs"

        /// "application/vnd.amazon.ebook": Amazon Kindle eBook format
        let vnd_amazon_ebook = MimeType "application/vnd.amazon.ebook"

        /// "application/vnd.americandynamics.acc": Active Content Compression
        let vnd_americandynamics_acc = MimeType "application/vnd.americandynamics.acc"

        /// "application/vnd.amiga.ami": AmigaDE
        let vnd_amiga_ami = MimeType "application/vnd.amiga.ami"

        /// "application/vnd.android.package-archive": Android Package Archive
        let vnd_android_package_archive = MimeType "application/vnd.android.package-archive"

        /// "application/vnd.anser-web-certificate-issue-initiation": ANSER-WEB Terminal Client - Certificate Issue
        let vnd_anser_web_certificate_issue_initiation = MimeType "application/vnd.anser-web-certificate-issue-initiation"

        /// "application/vnd.anser-web-funds-transfer-initiation": ANSER-WEB Terminal Client - Web Funds Transfer
        let vnd_anser_web_funds_transfer_initiation = MimeType "application/vnd.anser-web-funds-transfer-initiation"

        /// "application/vnd.antix.game-component": Antix Game Player
        let vnd_antix_game_component = MimeType "application/vnd.antix.game-component"

        /// "application/vnd.apple.installer+xml": Apple Installer Package
        let vnd_apple_installer_xml = MimeType "application/vnd.apple.installer+xml"

        /// "application/vnd.apple.mpegurl": Multimedia Playlist Unicode
        let vnd_apple_mpegurl = MimeType "application/vnd.apple.mpegurl"

        /// "application/vnd.aristanetworks.swi": Arista Networks Software Image
        let vnd_aristanetworks_swi = MimeType "application/vnd.aristanetworks.swi"

        /// "application/vnd.audiograph": Audiograph
        let vnd_audiograph = MimeType "application/vnd.audiograph"

        /// "application/vnd.blueice.multipass": Blueice Research Multipass
        let vnd_blueice_multipass = MimeType "application/vnd.blueice.multipass"

        /// "application/vnd.bmi": BMI Drawing Data Interchange
        let vnd_bmi = MimeType "application/vnd.bmi"

        /// "application/vnd.businessobjects": BusinessObjects
        let vnd_businessobjects = MimeType "application/vnd.businessobjects"

        /// "application/vnd.chemdraw+xml": CambridgeSoft Chem Draw
        let vnd_chemdraw_xml = MimeType "application/vnd.chemdraw+xml"

        /// "application/vnd.chipnuts.karaoke-mmd": Karaoke on Chipnuts Chipsets
        let vnd_chipnuts_karaoke_mmd = MimeType "application/vnd.chipnuts.karaoke-mmd"

        /// "application/vnd.cinderella": Interactive Geometry Software Cinderella
        let vnd_cinderella = MimeType "application/vnd.cinderella"

        /// "application/vnd.claymore": Claymore Data Files
        let vnd_claymore = MimeType "application/vnd.claymore"

        /// "application/vnd.cloanto.rp9": RetroPlatform Player
        let vnd_cloanto_rp9 = MimeType "application/vnd.cloanto.rp9"

        /// "application/vnd.clonk.c4group": Clonk Game
        let vnd_clonk_c4group = MimeType "application/vnd.clonk.c4group"

        /// "application/vnd.cluetrust.cartomobile-config": ClueTrust CartoMobile - Config
        let vnd_cluetrust_cartomobile_config = MimeType "application/vnd.cluetrust.cartomobile-config"

        /// "application/vnd.cluetrust.cartomobile-config-pkg": ClueTrust CartoMobile - Config Package
        let vnd_cluetrust_cartomobile_config_pkg = MimeType "application/vnd.cluetrust.cartomobile-config-pkg"

        /// "application/vnd.commonspace": Sixth Floor Media - CommonSpace
        let vnd_commonspace = MimeType "application/vnd.commonspace"

        /// "application/vnd.contact.cmsg": CIM Database
        let vnd_contact_cmsg = MimeType "application/vnd.contact.cmsg"

        /// "application/vnd.cosmocaller": CosmoCaller
        let vnd_cosmocaller = MimeType "application/vnd.cosmocaller"

        /// "application/vnd.crick.clicker": CrickSoftware - Clicker
        let vnd_crick_clicker = MimeType "application/vnd.crick.clicker"

        /// "application/vnd.crick.clicker.keyboard": CrickSoftware - Clicker - Keyboard
        let vnd_crick_clicker_keyboard = MimeType "application/vnd.crick.clicker.keyboard"

        /// "application/vnd.crick.clicker.palette": CrickSoftware - Clicker - Palette
        let vnd_crick_clicker_palette = MimeType "application/vnd.crick.clicker.palette"

        /// "application/vnd.crick.clicker.template": CrickSoftware - Clicker - Template
        let vnd_crick_clicker_template = MimeType "application/vnd.crick.clicker.template"

        /// "application/vnd.crick.clicker.wordbank": CrickSoftware - Clicker - Wordbank
        let vnd_crick_clicker_wordbank = MimeType "application/vnd.crick.clicker.wordbank"

        /// "application/vnd.criticaltools.wbs+xml": Critical Tools - PERT Chart EXPERT
        let vnd_criticaltools_wbs_xml = MimeType "application/vnd.criticaltools.wbs+xml"

        /// "application/vnd.ctc-posml": PosML
        let vnd_ctc_posml = MimeType "application/vnd.ctc-posml"

        /// "application/vnd.cups-ppd": Adobe PostScript Printer Description File Format
        let vnd_cups_ppd = MimeType "application/vnd.cups-ppd"

        /// "application/vnd.curl.car": CURL Applet
        let vnd_curl_car = MimeType "application/vnd.curl.car"

        /// "application/vnd.curl.pcurl": CURL Applet
        let vnd_curl_pcurl = MimeType "application/vnd.curl.pcurl"

        /// "application/vnd.data-vision.rdz": RemoteDocs R-Viewer
        let vnd_data_vision_rdz = MimeType "application/vnd.data-vision.rdz"

        /// "application/vnd.denovo.fcselayout-link": FCS Express Layout Link
        let vnd_denovo_fcselayout_link = MimeType "application/vnd.denovo.fcselayout-link"

        /// "application/vnd.dna": New Moon Liftoff/DNA
        let vnd_dna = MimeType "application/vnd.dna"

        /// "application/vnd.dolby.mlp": Dolby Meridian Lossless Packing
        let vnd_dolby_mlp = MimeType "application/vnd.dolby.mlp"

        /// "application/vnd.dpgraph": DPGraph
        let vnd_dpgraph = MimeType "application/vnd.dpgraph"

        /// "application/vnd.dreamfactory": DreamFactory
        let vnd_dreamfactory = MimeType "application/vnd.dreamfactory"

        /// "application/vnd.dvb.ait": Digital Video Broadcasting
        let vnd_dvb_ait = MimeType "application/vnd.dvb.ait"

        /// "application/vnd.dvb.service": Digital Video Broadcasting
        let vnd_dvb_service = MimeType "application/vnd.dvb.service"

        /// "application/vnd.dynageo": DynaGeo
        let vnd_dynageo = MimeType "application/vnd.dynageo"

        /// "application/vnd.ecowin.chart": EcoWin Chart
        let vnd_ecowin_chart = MimeType "application/vnd.ecowin.chart"

        /// "application/vnd.enliven": Enliven Viewer
        let vnd_enliven = MimeType "application/vnd.enliven"

        /// "application/vnd.epson.esf": QUASS Stream Player
        let vnd_epson_esf = MimeType "application/vnd.epson.esf"

        /// "application/vnd.epson.msf": QUASS Stream Player
        let vnd_epson_msf = MimeType "application/vnd.epson.msf"

        /// "application/vnd.epson.quickanime": QuickAnime Player
        let vnd_epson_quickanime = MimeType "application/vnd.epson.quickanime"

        /// "application/vnd.epson.salt": SimpleAnimeLite Player
        let vnd_epson_salt = MimeType "application/vnd.epson.salt"

        /// "application/vnd.epson.ssf": QUASS Stream Player
        let vnd_epson_ssf = MimeType "application/vnd.epson.ssf"

        /// "application/vnd.eszigno3+xml": MICROSEC e-Szign¢
        let vnd_eszigno3_xml = MimeType "application/vnd.eszigno3+xml"

        /// "application/vnd.ezpix-album": EZPix Secure Photo Album
        let vnd_ezpix_album = MimeType "application/vnd.ezpix-album"

        /// "application/vnd.ezpix-package": EZPix Secure Photo Album
        let vnd_ezpix_package = MimeType "application/vnd.ezpix-package"

        /// "application/vnd.fdf": Forms Data Format
        let vnd_fdf = MimeType "application/vnd.fdf"

        /// "application/vnd.fdsn.seed": Digital Siesmograph Networks - SEED Datafiles
        let vnd_fdsn_seed = MimeType "application/vnd.fdsn.seed"

        /// "application/vnd.flographit": NpGraphIt
        let vnd_flographit = MimeType "application/vnd.flographit"

        /// "application/vnd.fluxtime.clip": FluxTime Clip
        let vnd_fluxtime_clip = MimeType "application/vnd.fluxtime.clip"

        /// "application/vnd.framemaker": FrameMaker Normal Format
        let vnd_framemaker = MimeType "application/vnd.framemaker"

        /// "application/vnd.frogans.fnc": Frogans Player
        let vnd_frogans_fnc = MimeType "application/vnd.frogans.fnc"

        /// "application/vnd.frogans.ltf": Frogans Player
        let vnd_frogans_ltf = MimeType "application/vnd.frogans.ltf"

        /// "application/vnd.fsc.weblaunch": Friendly Software Corporation
        let vnd_fsc_weblaunch = MimeType "application/vnd.fsc.weblaunch"

        /// "application/vnd.fujitsu.oasys": Fujitsu Oasys
        let vnd_fujitsu_oasys = MimeType "application/vnd.fujitsu.oasys"

        /// "application/vnd.fujitsu.oasys2": Fujitsu Oasys
        let vnd_fujitsu_oasys2 = MimeType "application/vnd.fujitsu.oasys2"

        /// "application/vnd.fujitsu.oasys3": Fujitsu Oasys
        let vnd_fujitsu_oasys3 = MimeType "application/vnd.fujitsu.oasys3"

        /// "application/vnd.fujitsu.oasysgp": Fujitsu Oasys
        let vnd_fujitsu_oasysgp = MimeType "application/vnd.fujitsu.oasysgp"

        /// "application/vnd.fujitsu.oasysprs": Fujitsu Oasys
        let vnd_fujitsu_oasysprs = MimeType "application/vnd.fujitsu.oasysprs"

        /// "application/vnd.fujixerox.ddd": Fujitsu - Xerox 2D CAD Data
        let vnd_fujixerox_ddd = MimeType "application/vnd.fujixerox.ddd"

        /// "application/vnd.fujixerox.docuworks": Fujitsu - Xerox DocuWorks
        let vnd_fujixerox_docuworks = MimeType "application/vnd.fujixerox.docuworks"

        /// "application/vnd.fujixerox.docuworks.binder": Fujitsu - Xerox DocuWorks Binder
        let vnd_fujixerox_docuworks_binder = MimeType "application/vnd.fujixerox.docuworks.binder"

        /// "application/vnd.fuzzysheet": FuzzySheet
        let vnd_fuzzysheet = MimeType "application/vnd.fuzzysheet"

        /// "application/vnd.genomatix.tuxedo": Genomatix Tuxedo Framework
        let vnd_genomatix_tuxedo = MimeType "application/vnd.genomatix.tuxedo"

        /// "application/vnd.geogebra.file": GeoGebra
        let vnd_geogebra_file = MimeType "application/vnd.geogebra.file"

        /// "application/vnd.geogebra.tool": GeoGebra
        let vnd_geogebra_tool = MimeType "application/vnd.geogebra.tool"

        /// "application/vnd.geometry-explorer": GeoMetry Explorer
        let vnd_geometry_explorer = MimeType "application/vnd.geometry-explorer"

        /// "application/vnd.geonext": GEONExT and JSXGraph
        let vnd_geonext = MimeType "application/vnd.geonext"

        /// "application/vnd.geoplan": GeoplanW
        let vnd_geoplan = MimeType "application/vnd.geoplan"

        /// "application/vnd.geospace": GeospacW
        let vnd_geospace = MimeType "application/vnd.geospace"

        /// "application/vnd.gmx": GameMaker ActiveX
        let vnd_gmx = MimeType "application/vnd.gmx"

        /// "application/vnd.google-earth.kml+xml": Google Earth - KML
        let vnd_google_earth_kml_xml = MimeType "application/vnd.google-earth.kml+xml"

        /// "application/vnd.google-earth.kmz": Google Earth - Zipped KML
        let vnd_google_earth_kmz = MimeType "application/vnd.google-earth.kmz"

        /// "application/vnd.grafeq": GrafEq
        let vnd_grafeq = MimeType "application/vnd.grafeq"

        /// "application/vnd.groove-account": Groove - Account
        let vnd_groove_account = MimeType "application/vnd.groove-account"

        /// "application/vnd.groove-help": Groove - Help
        let vnd_groove_help = MimeType "application/vnd.groove-help"

        /// "application/vnd.groove-identity-message": Groove - Identity Message
        let vnd_groove_identity_message = MimeType "application/vnd.groove-identity-message"

        /// "application/vnd.groove-injector": Groove - Injector
        let vnd_groove_injector = MimeType "application/vnd.groove-injector"

        /// "application/vnd.groove-tool-message": Groove - Tool Message
        let vnd_groove_tool_message = MimeType "application/vnd.groove-tool-message"

        /// "application/vnd.groove-tool-template": Groove - Tool Template
        let vnd_groove_tool_template = MimeType "application/vnd.groove-tool-template"

        /// "application/vnd.groove-vcard": Groove - Vcard
        let vnd_groove_vcard = MimeType "application/vnd.groove-vcard"

        /// "application/vnd.hal+xml": Hypertext Application Language
        let vnd_hal_xml = MimeType "application/vnd.hal+xml"

        /// "application/vnd.handheld-entertainment+xml": ZVUE Media Manager
        let vnd_handheld_entertainment_xml = MimeType "application/vnd.handheld-entertainment+xml"

        /// "application/vnd.hbci": Homebanking Computer Interface (HBCI)
        let vnd_hbci = MimeType "application/vnd.hbci"

        /// "application/vnd.hhe.lesson-player": Archipelago Lesson Player
        let vnd_hhe_lesson_player = MimeType "application/vnd.hhe.lesson-player"

        /// "application/vnd.hp-hpgl": HP-GL/2 and HP RTL
        let vnd_hp_hpgl = MimeType "application/vnd.hp-hpgl"

        /// "application/vnd.hp-hpid": Hewlett Packard Instant Delivery
        let vnd_hp_hpid = MimeType "application/vnd.hp-hpid"

        /// "application/vnd.hp-hps": Hewlett-Packard's WebPrintSmart
        let vnd_hp_hps = MimeType "application/vnd.hp-hps"

        /// "application/vnd.hp-jlyt": HP Indigo Digital Press - Job Layout Languate
        let vnd_hp_jlyt = MimeType "application/vnd.hp-jlyt"

        /// "application/vnd.hp-pcl": HP Printer Command Language
        let vnd_hp_pcl = MimeType "application/vnd.hp-pcl"

        /// "application/vnd.hp-pclxl": PCL 6 Enhanced (Formely PCL XL)
        let vnd_hp_pclxl = MimeType "application/vnd.hp-pclxl"

        /// "application/vnd.hydrostatix.sof-data": Hydrostatix Master Suite
        let vnd_hydrostatix_sof_data = MimeType "application/vnd.hydrostatix.sof-data"

        /// "application/vnd.hzn-3d-crossword": 3D Crossword Plugin
        let vnd_hzn_3d_crossword = MimeType "application/vnd.hzn-3d-crossword"

        /// "application/vnd.ibm.minipay": MiniPay
        let vnd_ibm_minipay = MimeType "application/vnd.ibm.minipay"

        /// "application/vnd.ibm.modcap": MO:DCA-P
        let vnd_ibm_modcap = MimeType "application/vnd.ibm.modcap"

        /// "application/vnd.ibm.rights-management": IBM DB2 Rights Manager
        let vnd_ibm_rights_management = MimeType "application/vnd.ibm.rights-management"

        /// "application/vnd.ibm.secure-container": IBM Electronic Media Management System - Secure Container
        let vnd_ibm_secure_container = MimeType "application/vnd.ibm.secure-container"

        /// "application/vnd.iccprofile": ICC profile
        let vnd_iccprofile = MimeType "application/vnd.iccprofile"

        /// "application/vnd.igloader": igLoader
        let vnd_igloader = MimeType "application/vnd.igloader"

        /// "application/vnd.immervision-ivp": ImmerVision PURE Players
        let vnd_immervision_ivp = MimeType "application/vnd.immervision-ivp"

        /// "application/vnd.immervision-ivu": ImmerVision PURE Players
        let vnd_immervision_ivu = MimeType "application/vnd.immervision-ivu"

        /// "application/vnd.insors.igm": IOCOM Visimeet
        let vnd_insors_igm = MimeType "application/vnd.insors.igm"

        /// "application/vnd.intercon.formnet": Intercon FormNet
        let vnd_intercon_formnet = MimeType "application/vnd.intercon.formnet"

        /// "application/vnd.intergeo": Interactive Geometry Software
        let vnd_intergeo = MimeType "application/vnd.intergeo"

        /// "application/vnd.intu.qbo": Open Financial Exchange
        let vnd_intu_qbo = MimeType "application/vnd.intu.qbo"

        /// "application/vnd.intu.qfx": Quicken
        let vnd_intu_qfx = MimeType "application/vnd.intu.qfx"

        /// "application/vnd.ipunplugged.rcprofile": IP Unplugged Roaming Client
        let vnd_ipunplugged_rcprofile = MimeType "application/vnd.ipunplugged.rcprofile"

        /// "application/vnd.irepository.package+xml": iRepository / Lucidoc Editor
        let vnd_irepository_package_xml = MimeType "application/vnd.irepository.package+xml"

        /// "application/vnd.is-xpr": Express by Infoseek
        let vnd_is_xpr = MimeType "application/vnd.is-xpr"

        /// "application/vnd.isac.fcs": International Society for Advancement of Cytometry
        let vnd_isac_fcs = MimeType "application/vnd.isac.fcs"

        /// "application/vnd.jam": Lightspeed Audio Lab
        let vnd_jam = MimeType "application/vnd.jam"

        /// "application/vnd.jcp.javame.midlet-rms": Mobile Information Device Profile
        let vnd_jcp_javame_midlet_rms = MimeType "application/vnd.jcp.javame.midlet-rms"

        /// "application/vnd.jisp": RhymBox
        let vnd_jisp = MimeType "application/vnd.jisp"

        /// "application/vnd.joost.joda-archive": Joda Archive
        let vnd_joost_joda_archive = MimeType "application/vnd.joost.joda-archive"

        /// "application/vnd.kahootz": Kahootz
        let vnd_kahootz = MimeType "application/vnd.kahootz"

        /// "application/vnd.kde.karbon": KDE KOffice Office Suite - Karbon
        let vnd_kde_karbon = MimeType "application/vnd.kde.karbon"

        /// "application/vnd.kde.kchart": KDE KOffice Office Suite - KChart
        let vnd_kde_kchart = MimeType "application/vnd.kde.kchart"

        /// "application/vnd.kde.kformula": KDE KOffice Office Suite - Kformula
        let vnd_kde_kformula = MimeType "application/vnd.kde.kformula"

        /// "application/vnd.kde.kivio": KDE KOffice Office Suite - Kivio
        let vnd_kde_kivio = MimeType "application/vnd.kde.kivio"

        /// "application/vnd.kde.kontour": KDE KOffice Office Suite - Kontour
        let vnd_kde_kontour = MimeType "application/vnd.kde.kontour"

        /// "application/vnd.kde.kpresenter": KDE KOffice Office Suite - Kpresenter
        let vnd_kde_kpresenter = MimeType "application/vnd.kde.kpresenter"

        /// "application/vnd.kde.kspread": KDE KOffice Office Suite - Kspread
        let vnd_kde_kspread = MimeType "application/vnd.kde.kspread"

        /// "application/vnd.kde.kword": KDE KOffice Office Suite - Kword
        let vnd_kde_kword = MimeType "application/vnd.kde.kword"

        /// "application/vnd.kenameaapp": Kenamea App
        let vnd_kenameaapp = MimeType "application/vnd.kenameaapp"

        /// "application/vnd.kidspiration": Kidspiration
        let vnd_kidspiration = MimeType "application/vnd.kidspiration"

        /// "application/vnd.kinar": Kinar Applications
        let vnd_kinar = MimeType "application/vnd.kinar"

        /// "application/vnd.koan": SSEYO Koan Play File
        let vnd_koan = MimeType "application/vnd.koan"

        /// "application/vnd.kodak-descriptor": Kodak Storyshare
        let vnd_kodak_descriptor = MimeType "application/vnd.kodak-descriptor"

        /// "application/vnd.las.las+xml": Laser App Enterprise
        let vnd_las_las_xml = MimeType "application/vnd.las.las+xml"

        /// "application/vnd.llamagraphics.life-balance.desktop": Life Balance - Desktop Edition
        let vnd_llamagraphics_life_balance_desktop = MimeType "application/vnd.llamagraphics.life-balance.desktop"

        /// "application/vnd.llamagraphics.life-balance.exchange+xml": Life Balance - Exchange Format
        let vnd_llamagraphics_life_balance_exchange_xml = MimeType "application/vnd.llamagraphics.life-balance.exchange+xml"

        /// "application/vnd.lotus-1-2-3": Lotus 1-2-3
        let vnd_lotus_1_2_3 = MimeType "application/vnd.lotus-1-2-3"

        /// "application/vnd.lotus-approach": Lotus Approach
        let vnd_lotus_approach = MimeType "application/vnd.lotus-approach"

        /// "application/vnd.lotus-freelance": Lotus Freelance
        let vnd_lotus_freelance = MimeType "application/vnd.lotus-freelance"

        /// "application/vnd.lotus-notes": Lotus Notes
        let vnd_lotus_notes = MimeType "application/vnd.lotus-notes"

        /// "application/vnd.lotus-organizer": Lotus Organizer
        let vnd_lotus_organizer = MimeType "application/vnd.lotus-organizer"

        /// "application/vnd.lotus-screencam": Lotus Screencam
        let vnd_lotus_screencam = MimeType "application/vnd.lotus-screencam"

        /// "application/vnd.lotus-wordpro": Lotus Wordpro
        let vnd_lotus_wordpro = MimeType "application/vnd.lotus-wordpro"

        /// "application/vnd.macports.portpkg": MacPorts Port System
        let vnd_macports_portpkg = MimeType "application/vnd.macports.portpkg"

        /// "application/vnd.mcd": Micro CADAM Helix D&D
        let vnd_mcd = MimeType "application/vnd.mcd"

        /// "application/vnd.medcalcdata": MedCalc
        let vnd_medcalcdata = MimeType "application/vnd.medcalcdata"

        /// "application/vnd.mediastation.cdkey": MediaRemote
        let vnd_mediastation_cdkey = MimeType "application/vnd.mediastation.cdkey"

        /// "application/vnd.mfer": Medical Waveform Encoding Format
        let vnd_mfer = MimeType "application/vnd.mfer"

        /// "application/vnd.mfmp": Melody Format for Mobile Platform
        let vnd_mfmp = MimeType "application/vnd.mfmp"

        /// "application/vnd.micrografx.flo": Micrografx
        let vnd_micrografx_flo = MimeType "application/vnd.micrografx.flo"

        /// "application/vnd.micrografx.igx": Micrografx iGrafx Professional
        let vnd_micrografx_igx = MimeType "application/vnd.micrografx.igx"

        /// "application/vnd.mif": FrameMaker Interchange Format
        let vnd_mif = MimeType "application/vnd.mif"

        /// "application/vnd.mobius.daf": Mobius Management Systems - UniversalArchive
        let vnd_mobius_daf = MimeType "application/vnd.mobius.daf"

        /// "application/vnd.mobius.dis": Mobius Management Systems - Distribution Database
        let vnd_mobius_dis = MimeType "application/vnd.mobius.dis"

        /// "application/vnd.mobius.mbk": Mobius Management Systems - Basket file
        let vnd_mobius_mbk = MimeType "application/vnd.mobius.mbk"

        /// "application/vnd.mobius.mqy": Mobius Management Systems - Query File
        let vnd_mobius_mqy = MimeType "application/vnd.mobius.mqy"

        /// "application/vnd.mobius.msl": Mobius Management Systems - Script Language
        let vnd_mobius_msl = MimeType "application/vnd.mobius.msl"

        /// "application/vnd.mobius.plc": Mobius Management Systems - Policy Definition Language File
        let vnd_mobius_plc = MimeType "application/vnd.mobius.plc"

        /// "application/vnd.mobius.txf": Mobius Management Systems - Topic Index File
        let vnd_mobius_txf = MimeType "application/vnd.mobius.txf"

        /// "application/vnd.mophun.application": Mophun VM
        let vnd_mophun_application = MimeType "application/vnd.mophun.application"

        /// "application/vnd.mophun.certificate": Mophun Certificate
        let vnd_mophun_certificate = MimeType "application/vnd.mophun.certificate"

        /// "application/vnd.mozilla.xul+xml": XUL - XML User Interface Language
        let vnd_mozilla_xul_xml = MimeType "application/vnd.mozilla.xul+xml"

        /// "application/vnd.ms-artgalry": Microsoft Artgalry
        let vnd_ms_artgalry = MimeType "application/vnd.ms-artgalry"

        /// "application/vnd.ms-cab-compressed": Microsoft Cabinet File
        let vnd_ms_cab_compressed = MimeType "application/vnd.ms-cab-compressed"

        /// "application/vnd.ms-excel": Microsoft Excel
        let vnd_ms_excel = MimeType "application/vnd.ms-excel"

        /// "application/vnd.ms-excel.addin.macroenabled.12": Microsoft Excel - Add-In File
        let vnd_ms_excel_addin_macroenabled_12 = MimeType "application/vnd.ms-excel.addin.macroenabled.12"

        /// "application/vnd.ms-excel.sheet.binary.macroenabled.12": Microsoft Excel - Binary Workbook
        let vnd_ms_excel_sheet_binary_macroenabled_12 = MimeType "application/vnd.ms-excel.sheet.binary.macroenabled.12"

        /// "application/vnd.ms-excel.sheet.macroenabled.12": Microsoft Excel - Macro-Enabled Workbook
        let vnd_ms_excel_sheet_macroenabled_12 = MimeType "application/vnd.ms-excel.sheet.macroenabled.12"

        /// "application/vnd.ms-excel.template.macroenabled.12": Microsoft Excel - Macro-Enabled Template File
        let vnd_ms_excel_template_macroenabled_12 = MimeType "application/vnd.ms-excel.template.macroenabled.12"

        /// "application/vnd.ms-fontobject": Microsoft Embedded OpenType
        let vnd_ms_fontobject = MimeType "application/vnd.ms-fontobject"

        /// "application/vnd.ms-htmlhelp": Microsoft Html Help File
        let vnd_ms_htmlhelp = MimeType "application/vnd.ms-htmlhelp"

        /// "application/vnd.ms-ims": Microsoft Class Server
        let vnd_ms_ims = MimeType "application/vnd.ms-ims"

        /// "application/vnd.ms-lrm": Microsoft Learning Resource Module
        let vnd_ms_lrm = MimeType "application/vnd.ms-lrm"

        /// "application/vnd.ms-officetheme": Microsoft Office System Release Theme
        let vnd_ms_officetheme = MimeType "application/vnd.ms-officetheme"

        /// "application/vnd.ms-pki.seccat": Microsoft Trust UI Provider - Security Catalog
        let vnd_ms_pki_seccat = MimeType "application/vnd.ms-pki.seccat"

        /// "application/vnd.ms-pki.stl": Microsoft Trust UI Provider - Certificate Trust Link
        let vnd_ms_pki_stl = MimeType "application/vnd.ms-pki.stl"

        /// "application/vnd.ms-powerpoint": Microsoft PowerPoint
        let vnd_ms_powerpoint = MimeType "application/vnd.ms-powerpoint"

        /// "application/vnd.ms-powerpoint.addin.macroenabled.12": Microsoft PowerPoint - Add-in file
        let vnd_ms_powerpoint_addin_macroenabled_12 = MimeType "application/vnd.ms-powerpoint.addin.macroenabled.12"

        /// "application/vnd.ms-powerpoint.presentation.macroenabled.12": Microsoft PowerPoint - Macro-Enabled Presentation File
        let vnd_ms_powerpoint_presentation_macroenabled_12 = MimeType "application/vnd.ms-powerpoint.presentation.macroenabled.12"

        /// "application/vnd.ms-powerpoint.slide.macroenabled.12": Microsoft PowerPoint - Macro-Enabled Open XML Slide
        let vnd_ms_powerpoint_slide_macroenabled_12 = MimeType "application/vnd.ms-powerpoint.slide.macroenabled.12"

        /// "application/vnd.ms-powerpoint.slideshow.macroenabled.12": Microsoft PowerPoint - Macro-Enabled Slide Show File
        let vnd_ms_powerpoint_slideshow_macroenabled_12 = MimeType "application/vnd.ms-powerpoint.slideshow.macroenabled.12"

        /// "application/vnd.ms-powerpoint.template.macroenabled.12": Micosoft PowerPoint - Macro-Enabled Template File
        let vnd_ms_powerpoint_template_macroenabled_12 = MimeType "application/vnd.ms-powerpoint.template.macroenabled.12"

        /// "application/vnd.ms-project": Microsoft Project
        let vnd_ms_project = MimeType "application/vnd.ms-project"

        /// "application/vnd.ms-word.document.macroenabled.12": Micosoft Word - Macro-Enabled Document
        let vnd_ms_word_document_macroenabled_12 = MimeType "application/vnd.ms-word.document.macroenabled.12"

        /// "application/vnd.ms-word.template.macroenabled.12": Micosoft Word - Macro-Enabled Template
        let vnd_ms_word_template_macroenabled_12 = MimeType "application/vnd.ms-word.template.macroenabled.12"

        /// "application/vnd.ms-works": Microsoft Works
        let vnd_ms_works = MimeType "application/vnd.ms-works"

        /// "application/vnd.ms-wpl": Microsoft Windows Media Player Playlist
        let vnd_ms_wpl = MimeType "application/vnd.ms-wpl"

        /// "application/vnd.ms-xpsdocument": Microsoft XML Paper Specification
        let vnd_ms_xpsdocument = MimeType "application/vnd.ms-xpsdocument"

        /// "application/vnd.mseq": 3GPP MSEQ File
        let vnd_mseq = MimeType "application/vnd.mseq"

        /// "application/vnd.musician": MUsical Score Interpreted Code Invented  for the ASCII designation of Notation
        let vnd_musician = MimeType "application/vnd.musician"

        /// "application/vnd.muvee.style": Muvee Automatic Video Editing
        let vnd_muvee_style = MimeType "application/vnd.muvee.style"

        /// "application/vnd.neurolanguage.nlu": neuroLanguage
        let vnd_neurolanguage_nlu = MimeType "application/vnd.neurolanguage.nlu"

        /// "application/vnd.noblenet-directory": NobleNet Directory
        let vnd_noblenet_directory = MimeType "application/vnd.noblenet-directory"

        /// "application/vnd.noblenet-sealer": NobleNet Sealer
        let vnd_noblenet_sealer = MimeType "application/vnd.noblenet-sealer"

        /// "application/vnd.noblenet-web": NobleNet Web
        let vnd_noblenet_web = MimeType "application/vnd.noblenet-web"

        /// "application/vnd.nokia.n-gage.data": N-Gage Game Data
        let vnd_nokia_n_gage_data = MimeType "application/vnd.nokia.n-gage.data"

        /// "application/vnd.nokia.n-gage.symbian.install": N-Gage Game Installer
        let vnd_nokia_n_gage_symbian_install = MimeType "application/vnd.nokia.n-gage.symbian.install"

        /// "application/vnd.nokia.radio-preset": Nokia Radio Application - Preset
        let vnd_nokia_radio_preset = MimeType "application/vnd.nokia.radio-preset"

        /// "application/vnd.nokia.radio-presets": Nokia Radio Application - Preset
        let vnd_nokia_radio_presets = MimeType "application/vnd.nokia.radio-presets"

        /// "application/vnd.novadigm.edm": Novadigm's RADIA and EDM products
        let vnd_novadigm_edm = MimeType "application/vnd.novadigm.edm"

        /// "application/vnd.novadigm.edx": Novadigm's RADIA and EDM products
        let vnd_novadigm_edx = MimeType "application/vnd.novadigm.edx"

        /// "application/vnd.novadigm.ext": Novadigm's RADIA and EDM products
        let vnd_novadigm_ext = MimeType "application/vnd.novadigm.ext"

        /// "application/vnd.oasis.opendocument.chart": OpenDocument Chart
        let vnd_oasis_opendocument_chart = MimeType "application/vnd.oasis.opendocument.chart"

        /// "application/vnd.oasis.opendocument.chart-template": OpenDocument Chart Template
        let vnd_oasis_opendocument_chart_template = MimeType "application/vnd.oasis.opendocument.chart-template"

        /// "application/vnd.oasis.opendocument.database": OpenDocument Database
        let vnd_oasis_opendocument_database = MimeType "application/vnd.oasis.opendocument.database"

        /// "application/vnd.oasis.opendocument.formula": OpenDocument Formula
        let vnd_oasis_opendocument_formula = MimeType "application/vnd.oasis.opendocument.formula"

        /// "application/vnd.oasis.opendocument.formula-template": OpenDocument Formula Template
        let vnd_oasis_opendocument_formula_template = MimeType "application/vnd.oasis.opendocument.formula-template"

        /// "application/vnd.oasis.opendocument.graphics": OpenDocument Graphics
        let vnd_oasis_opendocument_graphics = MimeType "application/vnd.oasis.opendocument.graphics"

        /// "application/vnd.oasis.opendocument.graphics-template": OpenDocument Graphics Template
        let vnd_oasis_opendocument_graphics_template = MimeType "application/vnd.oasis.opendocument.graphics-template"

        /// "application/vnd.oasis.opendocument.image": OpenDocument Image
        let vnd_oasis_opendocument_image = MimeType "application/vnd.oasis.opendocument.image"

        /// "application/vnd.oasis.opendocument.image-template": OpenDocument Image Template
        let vnd_oasis_opendocument_image_template = MimeType "application/vnd.oasis.opendocument.image-template"

        /// "application/vnd.oasis.opendocument.presentation": OpenDocument Presentation
        let vnd_oasis_opendocument_presentation = MimeType "application/vnd.oasis.opendocument.presentation"

        /// "application/vnd.oasis.opendocument.presentation-template": OpenDocument Presentation Template
        let vnd_oasis_opendocument_presentation_template = MimeType "application/vnd.oasis.opendocument.presentation-template"

        /// "application/vnd.oasis.opendocument.spreadsheet": OpenDocument Spreadsheet
        let vnd_oasis_opendocument_spreadsheet = MimeType "application/vnd.oasis.opendocument.spreadsheet"

        /// "application/vnd.oasis.opendocument.spreadsheet-template": OpenDocument Spreadsheet Template
        let vnd_oasis_opendocument_spreadsheet_template = MimeType "application/vnd.oasis.opendocument.spreadsheet-template"

        /// "application/vnd.oasis.opendocument.text": OpenDocument Text
        let vnd_oasis_opendocument_text = MimeType "application/vnd.oasis.opendocument.text"

        /// "application/vnd.oasis.opendocument.text-master": OpenDocument Text Master
        let vnd_oasis_opendocument_text_master = MimeType "application/vnd.oasis.opendocument.text-master"

        /// "application/vnd.oasis.opendocument.text-template": OpenDocument Text Template
        let vnd_oasis_opendocument_text_template = MimeType "application/vnd.oasis.opendocument.text-template"

        /// "application/vnd.oasis.opendocument.text-web": Open Document Text Web
        let vnd_oasis_opendocument_text_web = MimeType "application/vnd.oasis.opendocument.text-web"

        /// "application/vnd.olpc-sugar": Sugar Linux Application Bundle
        let vnd_olpc_sugar = MimeType "application/vnd.olpc-sugar"

        /// "application/vnd.oma.dd2+xml": OMA Download Agents
        let vnd_oma_dd2_xml = MimeType "application/vnd.oma.dd2+xml"

        /// "application/vnd.openofficeorg.extension": Open Office Extension
        let vnd_openofficeorg_extension = MimeType "application/vnd.openofficeorg.extension"

        /// "application/vnd.openxmlformats-officedocument.presentationml.presentation": Microsoft Office - OOXML - Presentation
        let vnd_openxmlformats_officedocument_presentationml_presentation = MimeType "application/vnd.openxmlformats-officedocument.presentationml.presentation"

        /// "application/vnd.openxmlformats-officedocument.presentationml.slide": Microsoft Office - OOXML - Presentation (Slide)
        let vnd_openxmlformats_officedocument_presentationml_slide = MimeType "application/vnd.openxmlformats-officedocument.presentationml.slide"

        /// "application/vnd.openxmlformats-officedocument.presentationml.slideshow": Microsoft Office - OOXML - Presentation (Slideshow)
        let vnd_openxmlformats_officedocument_presentationml_slideshow = MimeType "application/vnd.openxmlformats-officedocument.presentationml.slideshow"

        /// "application/vnd.openxmlformats-officedocument.presentationml.template": Microsoft Office - OOXML - Presentation Template
        let vnd_openxmlformats_officedocument_presentationml_template = MimeType "application/vnd.openxmlformats-officedocument.presentationml.template"

        /// "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet": Microsoft Office - OOXML - Spreadsheet
        let vnd_openxmlformats_officedocument_spreadsheetml_sheet = MimeType "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"

        /// "application/vnd.openxmlformats-officedocument.spreadsheetml.template": Microsoft Office - OOXML - Spreadsheet Teplate
        let vnd_openxmlformats_officedocument_spreadsheetml_template = MimeType "application/vnd.openxmlformats-officedocument.spreadsheetml.template"

        /// "application/vnd.openxmlformats-officedocument.wordprocessingml.document": Microsoft Office - OOXML - Word Document
        let vnd_openxmlformats_officedocument_wordprocessingml_document = MimeType "application/vnd.openxmlformats-officedocument.wordprocessingml.document"

        /// "application/vnd.openxmlformats-officedocument.wordprocessingml.template": Microsoft Office - OOXML - Word Document Template
        let vnd_openxmlformats_officedocument_wordprocessingml_template = MimeType "application/vnd.openxmlformats-officedocument.wordprocessingml.template"

        /// "application/vnd.osgeo.mapguide.package": MapGuide DBXML
        let vnd_osgeo_mapguide_package = MimeType "application/vnd.osgeo.mapguide.package"

        /// "application/vnd.osgi.dp": OSGi Deployment Package
        let vnd_osgi_dp = MimeType "application/vnd.osgi.dp"

        /// "application/vnd.palm": PalmOS Data
        let vnd_palm = MimeType "application/vnd.palm"

        /// "application/vnd.pawaafile": PawaaFILE
        let vnd_pawaafile = MimeType "application/vnd.pawaafile"

        /// "application/vnd.pg.format": Proprietary P&G Standard Reporting System
        let vnd_pg_format = MimeType "application/vnd.pg.format"

        /// "application/vnd.pg.osasli": Proprietary P&G Standard Reporting System
        let vnd_pg_osasli = MimeType "application/vnd.pg.osasli"

        /// "application/vnd.picsel": Pcsel eFIF File
        let vnd_picsel = MimeType "application/vnd.picsel"

        /// "application/vnd.pmi.widget": Qualcomm's Plaza Mobile Internet
        let vnd_pmi_widget = MimeType "application/vnd.pmi.widget"

        /// "application/vnd.pocketlearn": PocketLearn Viewers
        let vnd_pocketlearn = MimeType "application/vnd.pocketlearn"

        /// "application/vnd.powerbuilder6": PowerBuilder
        let vnd_powerbuilder6 = MimeType "application/vnd.powerbuilder6"

        /// "application/vnd.previewsystems.box": Preview Systems ZipLock/VBox
        let vnd_previewsystems_box = MimeType "application/vnd.previewsystems.box"

        /// "application/vnd.proteus.magazine": EFI Proteus
        let vnd_proteus_magazine = MimeType "application/vnd.proteus.magazine"

        /// "application/vnd.publishare-delta-tree": PubliShare Objects
        let vnd_publishare_delta_tree = MimeType "application/vnd.publishare-delta-tree"

        /// "application/vnd.pvi.ptid1": Princeton Video Image
        let vnd_pvi_ptid1 = MimeType "application/vnd.pvi.ptid1"

        /// "application/vnd.quark.quarkxpress": QuarkXpress
        let vnd_quark_quarkxpress = MimeType "application/vnd.quark.quarkxpress"

        /// "application/vnd.realvnc.bed": RealVNC
        let vnd_realvnc_bed = MimeType "application/vnd.realvnc.bed"

        /// "application/vnd.recordare.musicxml": Recordare Applications
        let vnd_recordare_musicxml = MimeType "application/vnd.recordare.musicxml"

        /// "application/vnd.recordare.musicxml+xml": Recordare Applications
        let vnd_recordare_musicxml_xml = MimeType "application/vnd.recordare.musicxml+xml"

        /// "application/vnd.rig.cryptonote": CryptoNote
        let vnd_rig_cryptonote = MimeType "application/vnd.rig.cryptonote"

        /// "application/vnd.rim.cod": Blackberry COD File
        let vnd_rim_cod = MimeType "application/vnd.rim.cod"

        /// "application/vnd.rn-realmedia": RealMedia
        let vnd_rn_realmedia = MimeType "application/vnd.rn-realmedia"

        /// "application/vnd.route66.link66+xml": ROUTE 66 Location Based Services
        let vnd_route66_link66_xml = MimeType "application/vnd.route66.link66+xml"

        /// "application/vnd.sailingtracker.track": SailingTracker
        let vnd_sailingtracker_track = MimeType "application/vnd.sailingtracker.track"

        /// "application/vnd.seemail": SeeMail
        let vnd_seemail = MimeType "application/vnd.seemail"

        /// "application/vnd.sema": Secured eMail
        let vnd_sema = MimeType "application/vnd.sema"

        /// "application/vnd.semd": Secured eMail
        let vnd_semd = MimeType "application/vnd.semd"

        /// "application/vnd.semf": Secured eMail
        let vnd_semf = MimeType "application/vnd.semf"

        /// "application/vnd.shana.informed.formdata": Shana Informed Filler
        let vnd_shana_informed_formdata = MimeType "application/vnd.shana.informed.formdata"

        /// "application/vnd.shana.informed.formtemplate": Shana Informed Filler
        let vnd_shana_informed_formtemplate = MimeType "application/vnd.shana.informed.formtemplate"

        /// "application/vnd.shana.informed.interchange": Shana Informed Filler
        let vnd_shana_informed_interchange = MimeType "application/vnd.shana.informed.interchange"

        /// "application/vnd.shana.informed.package": Shana Informed Filler
        let vnd_shana_informed_package = MimeType "application/vnd.shana.informed.package"

        /// "application/vnd.simtech-mindmapper": SimTech MindMapper
        let vnd_simtech_mindmapper = MimeType "application/vnd.simtech-mindmapper"

        /// "application/vnd.smaf": SMAF File
        let vnd_smaf = MimeType "application/vnd.smaf"

        /// "application/vnd.smart.teacher": SMART Technologies Apps
        let vnd_smart_teacher = MimeType "application/vnd.smart.teacher"

        /// "application/vnd.solent.sdkm+xml": SudokuMagic
        let vnd_solent_sdkm_xml = MimeType "application/vnd.solent.sdkm+xml"

        /// "application/vnd.spotfire.dxp": TIBCO Spotfire
        let vnd_spotfire_dxp = MimeType "application/vnd.spotfire.dxp"

        /// "application/vnd.spotfire.sfs": TIBCO Spotfire
        let vnd_spotfire_sfs = MimeType "application/vnd.spotfire.sfs"

        /// "application/vnd.stardivision.calc": StarOffice - Calc
        let vnd_stardivision_calc = MimeType "application/vnd.stardivision.calc"

        /// "application/vnd.stardivision.draw": StarOffice - Draw
        let vnd_stardivision_draw = MimeType "application/vnd.stardivision.draw"

        /// "application/vnd.stardivision.impress": StarOffice - Impress
        let vnd_stardivision_impress = MimeType "application/vnd.stardivision.impress"

        /// "application/vnd.stardivision.math": StarOffice - Math
        let vnd_stardivision_math = MimeType "application/vnd.stardivision.math"

        /// "application/vnd.stardivision.writer": StarOffice - Writer
        let vnd_stardivision_writer = MimeType "application/vnd.stardivision.writer"

        /// "application/vnd.stardivision.writer-global": StarOffice - Writer  (Global)
        let vnd_stardivision_writer_global = MimeType "application/vnd.stardivision.writer-global"

        /// "application/vnd.stepmania.stepchart": StepMania
        let vnd_stepmania_stepchart = MimeType "application/vnd.stepmania.stepchart"

        /// "application/vnd.sun.xml.calc": OpenOffice - Calc (Spreadsheet)
        let vnd_sun_xml_calc = MimeType "application/vnd.sun.xml.calc"

        /// "application/vnd.sun.xml.calc.template": OpenOffice - Calc Template (Spreadsheet)
        let vnd_sun_xml_calc_template = MimeType "application/vnd.sun.xml.calc.template"

        /// "application/vnd.sun.xml.draw": OpenOffice - Draw (Graphics)
        let vnd_sun_xml_draw = MimeType "application/vnd.sun.xml.draw"

        /// "application/vnd.sun.xml.draw.template": OpenOffice - Draw Template (Graphics)
        let vnd_sun_xml_draw_template = MimeType "application/vnd.sun.xml.draw.template"

        /// "application/vnd.sun.xml.impress": OpenOffice - Impress (Presentation)
        let vnd_sun_xml_impress = MimeType "application/vnd.sun.xml.impress"

        /// "application/vnd.sun.xml.impress.template": OpenOffice - Impress Template (Presentation)
        let vnd_sun_xml_impress_template = MimeType "application/vnd.sun.xml.impress.template"

        /// "application/vnd.sun.xml.math": OpenOffice - Math (Formula)
        let vnd_sun_xml_math = MimeType "application/vnd.sun.xml.math"

        /// "application/vnd.sun.xml.writer": OpenOffice - Writer (Text - HTML)
        let vnd_sun_xml_writer = MimeType "application/vnd.sun.xml.writer"

        /// "application/vnd.sun.xml.writer.global": OpenOffice - Writer (Text - HTML)
        let vnd_sun_xml_writer_global = MimeType "application/vnd.sun.xml.writer.global"

        /// "application/vnd.sun.xml.writer.template": OpenOffice - Writer Template (Text - HTML)
        let vnd_sun_xml_writer_template = MimeType "application/vnd.sun.xml.writer.template"

        /// "application/vnd.sus-calendar": ScheduleUs
        let vnd_sus_calendar = MimeType "application/vnd.sus-calendar"

        /// "application/vnd.svd": SourceView Document
        let vnd_svd = MimeType "application/vnd.svd"

        /// "application/vnd.symbian.install": Symbian Install Package
        let vnd_symbian_install = MimeType "application/vnd.symbian.install"

        /// "application/vnd.syncml+xml": SyncML
        let vnd_syncml_xml = MimeType "application/vnd.syncml+xml"

        /// "application/vnd.syncml.dm+wbxml": SyncML - Device Management
        let vnd_syncml_dm_wbxml = MimeType "application/vnd.syncml.dm+wbxml"

        /// "application/vnd.syncml.dm+xml": SyncML - Device Management
        let vnd_syncml_dm_xml = MimeType "application/vnd.syncml.dm+xml"

        /// "application/vnd.tao.intent-module-archive": Tao Intent
        let vnd_tao_intent_module_archive = MimeType "application/vnd.tao.intent-module-archive"

        /// "application/vnd.tmobile-livetv": MobileTV
        let vnd_tmobile_livetv = MimeType "application/vnd.tmobile-livetv"

        /// "application/vnd.trid.tpt": TRI Systems Config
        let vnd_trid_tpt = MimeType "application/vnd.trid.tpt"

        /// "application/vnd.triscape.mxs": Triscape Map Explorer
        let vnd_triscape_mxs = MimeType "application/vnd.triscape.mxs"

        /// "application/vnd.trueapp": True BASIC
        let vnd_trueapp = MimeType "application/vnd.trueapp"

        /// "application/vnd.ufdl": Universal Forms Description Language
        let vnd_ufdl = MimeType "application/vnd.ufdl"

        /// "application/vnd.uiq.theme": User Interface Quartz - Theme (Symbian)
        let vnd_uiq_theme = MimeType "application/vnd.uiq.theme"

        /// "application/vnd.umajin": UMAJIN
        let vnd_umajin = MimeType "application/vnd.umajin"

        /// "application/vnd.unity": Unity 3d
        let vnd_unity = MimeType "application/vnd.unity"

        /// "application/vnd.uoml+xml": Unique Object Markup Language
        let vnd_uoml_xml = MimeType "application/vnd.uoml+xml"

        /// "application/vnd.vcx": VirtualCatalog
        let vnd_vcx = MimeType "application/vnd.vcx"

        /// "application/vnd.visio": Microsoft Visio
        let vnd_visio = MimeType "application/vnd.visio"

        /// "application/vnd.visionary": Visionary
        let vnd_visionary = MimeType "application/vnd.visionary"

        /// "application/vnd.vsf": Viewport+
        let vnd_vsf = MimeType "application/vnd.vsf"

        /// "application/vnd.wap.wbxml": WAP Binary XML (WBXML)
        let vnd_wap_wbxml = MimeType "application/vnd.wap.wbxml"

        /// "application/vnd.wap.wmlc": Compiled Wireless Markup Language (WMLC)
        let vnd_wap_wmlc = MimeType "application/vnd.wap.wmlc"

        /// "application/vnd.wap.wmlscriptc": WMLScript
        let vnd_wap_wmlscriptc = MimeType "application/vnd.wap.wmlscriptc"

        /// "application/vnd.webturbo": WebTurbo
        let vnd_webturbo = MimeType "application/vnd.webturbo"

        /// "application/vnd.wolfram.player": Mathematica Notebook Player
        let vnd_wolfram_player = MimeType "application/vnd.wolfram.player"

        /// "application/vnd.wordperfect": Wordperfect
        let vnd_wordperfect = MimeType "application/vnd.wordperfect"

        /// "application/vnd.wqd": SundaHus WQ
        let vnd_wqd = MimeType "application/vnd.wqd"

        /// "application/vnd.wt.stf": Worldtalk
        let vnd_wt_stf = MimeType "application/vnd.wt.stf"

        /// "application/vnd.xara": CorelXARA
        let vnd_xara = MimeType "application/vnd.xara"

        /// "application/vnd.xfdl": Extensible Forms Description Language
        let vnd_xfdl = MimeType "application/vnd.xfdl"

        /// "application/vnd.yamaha.hv-dic": HV Voice Dictionary
        let vnd_yamaha_hv_dic = MimeType "application/vnd.yamaha.hv-dic"

        /// "application/vnd.yamaha.hv-script": HV Script
        let vnd_yamaha_hv_script = MimeType "application/vnd.yamaha.hv-script"

        /// "application/vnd.yamaha.hv-voice": HV Voice Parameter
        let vnd_yamaha_hv_voice = MimeType "application/vnd.yamaha.hv-voice"

        /// "application/vnd.yamaha.openscoreformat": Open Score Format
        let vnd_yamaha_openscoreformat = MimeType "application/vnd.yamaha.openscoreformat"

        /// "application/vnd.yamaha.openscoreformat.osfpvg+xml": OSFPVG
        let vnd_yamaha_openscoreformat_osfpvg_xml = MimeType "application/vnd.yamaha.openscoreformat.osfpvg+xml"

        /// "application/vnd.yamaha.smaf-audio": SMAF Audio
        let vnd_yamaha_smaf_audio = MimeType "application/vnd.yamaha.smaf-audio"

        /// "application/vnd.yamaha.smaf-phrase": SMAF Phrase
        let vnd_yamaha_smaf_phrase = MimeType "application/vnd.yamaha.smaf-phrase"

        /// "application/vnd.yellowriver-custom-menu": CustomMenu
        let vnd_yellowriver_custom_menu = MimeType "application/vnd.yellowriver-custom-menu"

        /// "application/vnd.zul": Z.U.L. Geometry
        let vnd_zul = MimeType "application/vnd.zul"

        /// "application/vnd.zzazz.deck+xml": Zzazz Deck
        let vnd_zzazz_deck_xml = MimeType "application/vnd.zzazz.deck+xml"

        /// "application/voicexml+xml": VoiceXML
        let voicexml_xml = MimeType "application/voicexml+xml"

        /// "application/widget": Widget Packaging and XML Configuration
        let widget = MimeType "application/widget"

        /// "application/winhlp": WinHelp
        let winhlp = MimeType "application/winhlp"

        /// "application/wsdl+xml": WSDL - Web Services Description Language
        let wsdl_xml = MimeType "application/wsdl+xml"

        /// "application/wspolicy+xml": Web Services Policy
        let wspolicy_xml = MimeType "application/wspolicy+xml"

        /// "application/x-7z-compressed": 7-Zip
        let x_7z_compressed = MimeType "application/x-7z-compressed"

        /// "application/x-abiword": AbiWord
        let x_abiword = MimeType "application/x-abiword"

        /// "application/x-ace-compressed": Ace Archive
        let x_ace_compressed = MimeType "application/x-ace-compressed"

        /// "application/x-authorware-bin": Adobe (Macropedia) Authorware - Binary File
        let x_authorware_bin = MimeType "application/x-authorware-bin"

        /// "application/x-authorware-map": Adobe (Macropedia) Authorware - Map
        let x_authorware_map = MimeType "application/x-authorware-map"

        /// "application/x-authorware-seg": Adobe (Macropedia) Authorware - Segment File
        let x_authorware_seg = MimeType "application/x-authorware-seg"

        /// "application/x-bcpio": Binary CPIO Archive
        let x_bcpio = MimeType "application/x-bcpio"

        /// "application/x-bittorrent": BitTorrent
        let x_bittorrent = MimeType "application/x-bittorrent"

        /// "application/x-bzip": Bzip Archive
        let x_bzip = MimeType "application/x-bzip"

        /// "application/x-bzip2": Bzip2 Archive
        let x_bzip2 = MimeType "application/x-bzip2"

        /// "application/x-cdlink": Video CD
        let x_cdlink = MimeType "application/x-cdlink"

        /// "application/x-chat": pIRCh
        let x_chat = MimeType "application/x-chat"

        /// "application/x-chess-pgn": Portable Game Notation (Chess Games)
        let x_chess_pgn = MimeType "application/x-chess-pgn"

        /// "application/x-cpio": CPIO Archive
        let x_cpio = MimeType "application/x-cpio"

        /// "application/x-csh": C Shell Script
        let x_csh = MimeType "application/x-csh"

        /// "application/x-debian-package": Debian Package
        let x_debian_package = MimeType "application/x-debian-package"

        /// "application/x-director": Adobe Shockwave Player
        let x_director = MimeType "application/x-director"

        /// "application/x-doom": Doom Video Game
        let x_doom = MimeType "application/x-doom"

        /// "application/x-dtbncx+xml": Navigation Control file for XML (for ePub)
        let x_dtbncx_xml = MimeType "application/x-dtbncx+xml"

        /// "application/x-dtbook+xml": Digital Talking Book
        let x_dtbook_xml = MimeType "application/x-dtbook+xml"

        /// "application/x-dtbresource+xml": Digital Talking Book - Resource File
        let x_dtbresource_xml = MimeType "application/x-dtbresource+xml"

        /// "application/x-dvi": Device Independent File Format (DVI)
        let x_dvi = MimeType "application/x-dvi"

        /// "application/x-font-bdf": Glyph Bitmap Distribution Format
        let x_font_bdf = MimeType "application/x-font-bdf"

        /// "application/x-font-ghostscript": Ghostscript Font
        let x_font_ghostscript = MimeType "application/x-font-ghostscript"

        /// "application/x-font-linux-psf": PSF Fonts
        let x_font_linux_psf = MimeType "application/x-font-linux-psf"

        /// "application/x-font-otf": OpenType Font File
        let x_font_otf = MimeType "application/x-font-otf"

        /// "application/x-font-pcf": Portable Compiled Format
        let x_font_pcf = MimeType "application/x-font-pcf"

        /// "application/x-font-snf": Server Normal Format
        let x_font_snf = MimeType "application/x-font-snf"

        /// "application/x-font-ttf": TrueType Font
        let x_font_ttf = MimeType "application/x-font-ttf"

        /// "application/x-font-type1": PostScript Fonts
        let x_font_type1 = MimeType "application/x-font-type1"

        /// "application/x-font-woff": Web Open Font Format
        let x_font_woff = MimeType "application/x-font-woff"

        /// "application/x-futuresplash": FutureSplash Animator
        let x_futuresplash = MimeType "application/x-futuresplash"

        /// "application/x-gnumeric": Gnumeric
        let x_gnumeric = MimeType "application/x-gnumeric"

        /// "application/x-gtar": GNU Tar Files
        let x_gtar = MimeType "application/x-gtar"

        /// "application/x-hdf": Hierarchical Data Format
        let x_hdf = MimeType "application/x-hdf"

        /// "application/x-java-jnlp-file": Java Network Launching Protocol
        let x_java_jnlp_file = MimeType "application/x-java-jnlp-file"

        /// "application/x-latex": LaTeX
        let x_latex = MimeType "application/x-latex"

        /// "application/x-mobipocket-ebook": Mobipocket
        let x_mobipocket_ebook = MimeType "application/x-mobipocket-ebook"

        /// "application/x-ms-application": Microsoft ClickOnce
        let x_ms_application = MimeType "application/x-ms-application"

        /// "application/x-ms-wmd": Microsoft Windows Media Player Download Package
        let x_ms_wmd = MimeType "application/x-ms-wmd"

        /// "application/x-ms-wmz": Microsoft Windows Media Player Skin Package
        let x_ms_wmz = MimeType "application/x-ms-wmz"

        /// "application/x-ms-xbap": Microsoft XAML Browser Application
        let x_ms_xbap = MimeType "application/x-ms-xbap"

        /// "application/x-msaccess": Microsoft Access
        let x_msaccess = MimeType "application/x-msaccess"

        /// "application/x-msbinder": Microsoft Office Binder
        let x_msbinder = MimeType "application/x-msbinder"

        /// "application/x-mscardfile": Microsoft Information Card
        let x_mscardfile = MimeType "application/x-mscardfile"

        /// "application/x-msclip": Microsoft Clipboard Clip
        let x_msclip = MimeType "application/x-msclip"

        /// "application/x-msdownload": Microsoft Application
        let x_msdownload = MimeType "application/x-msdownload"

        /// "application/x-msmediaview": Microsoft MediaView
        let x_msmediaview = MimeType "application/x-msmediaview"

        /// "application/x-msmetafile": Microsoft Windows Metafile
        let x_msmetafile = MimeType "application/x-msmetafile"

        /// "application/x-msmoney": Microsoft Money
        let x_msmoney = MimeType "application/x-msmoney"

        /// "application/x-mspublisher": Microsoft Publisher
        let x_mspublisher = MimeType "application/x-mspublisher"

        /// "application/x-msschedule": Microsoft Schedule+
        let x_msschedule = MimeType "application/x-msschedule"

        /// "application/x-msterminal": Microsoft Windows Terminal Services
        let x_msterminal = MimeType "application/x-msterminal"

        /// "application/x-mswrite": Microsoft Wordpad
        let x_mswrite = MimeType "application/x-mswrite"

        /// "application/x-netcdf": Network Common Data Form (NetCDF)
        let x_netcdf = MimeType "application/x-netcdf"

        /// "application/x-pkcs12": PKCS #12 - Personal Information Exchange Syntax Standard
        let x_pkcs12 = MimeType "application/x-pkcs12"

        /// "application/x-pkcs7-certificates": PKCS #7 - Cryptographic Message Syntax Standard (Certificates)
        let x_pkcs7_certificates = MimeType "application/x-pkcs7-certificates"

        /// "application/x-pkcs7-certreqresp": PKCS #7 - Cryptographic Message Syntax Standard (Certificate Request Response)
        let x_pkcs7_certreqresp = MimeType "application/x-pkcs7-certreqresp"

        /// "application/x-rar-compressed": RAR Archive
        let x_rar_compressed = MimeType "application/x-rar-compressed"

        /// "application/x-sh": Bourne Shell Script
        let x_sh = MimeType "application/x-sh"

        /// "application/x-shar": Shell Archive
        let x_shar = MimeType "application/x-shar"

        /// "application/x-shockwave-flash": Adobe Flash
        let x_shockwave_flash = MimeType "application/x-shockwave-flash"

        /// "application/x-silverlight-app": Microsoft Silverlight
        let x_silverlight_app = MimeType "application/x-silverlight-app"

        /// "application/x-stuffit": Stuffit Archive
        let x_stuffit = MimeType "application/x-stuffit"

        /// "application/x-stuffitx": Stuffit Archive
        let x_stuffitx = MimeType "application/x-stuffitx"

        /// "application/x-sv4cpio": System V Release 4 CPIO Archive
        let x_sv4cpio = MimeType "application/x-sv4cpio"

        /// "application/x-sv4crc": System V Release 4 CPIO Checksum Data
        let x_sv4crc = MimeType "application/x-sv4crc"

        /// "application/x-tar": Tar File (Tape Archive)
        let x_tar = MimeType "application/x-tar"

        /// "application/x-tcl": Tcl Script
        let x_tcl = MimeType "application/x-tcl"

        /// "application/x-tex": TeX
        let x_tex = MimeType "application/x-tex"

        /// "application/x-tex-tfm": TeX Font Metric
        let x_tex_tfm = MimeType "application/x-tex-tfm"

        /// "application/x-texinfo": GNU Texinfo Document
        let x_texinfo = MimeType "application/x-texinfo"

        /// "application/x-ustar": Ustar (Uniform Standard Tape Archive)
        let x_ustar = MimeType "application/x-ustar"

        /// "application/x-wais-source": WAIS Source
        let x_wais_source = MimeType "application/x-wais-source"

        /// "application/x-x509-ca-cert": X.509 Certificate
        let x_x509_ca_cert = MimeType "application/x-x509-ca-cert"

        /// "application/x-xfig": Xfig
        let x_xfig = MimeType "application/x-xfig"

        /// "application/x-xpinstall": XPInstall - Mozilla
        let x_xpinstall = MimeType "application/x-xpinstall"

        /// "application/xcap-diff+xml": XML Configuration Access Protocol - XCAP Diff
        let xcap_diff_xml = MimeType "application/xcap-diff+xml"

        /// "application/xenc+xml": XML Encryption Syntax and Processing
        let xenc_xml = MimeType "application/xenc+xml"

        /// "application/xhtml+xml": XHTML - The Extensible HyperText Markup Language
        let xhtml_xml = MimeType "application/xhtml+xml"

        /// "application/xml": XML - Extensible Markup Language
        let xml = MimeType "application/xml"

        /// "application/xml-dtd": Document Type Definition
        let xml_dtd = MimeType "application/xml-dtd"

        /// "application/xop+xml": XML-Binary Optimized Packaging
        let xop_xml = MimeType "application/xop+xml"

        /// "application/xslt+xml": XML Transformations
        let xslt_xml = MimeType "application/xslt+xml"

        /// "application/xspf+xml": XSPF - XML Shareable Playlist Format
        let xspf_xml = MimeType "application/xspf+xml"

        /// "application/xv+xml": MXML
        let xv_xml = MimeType "application/xv+xml"

        /// "application/yang": YANG Data Modeling Language
        let yang = MimeType "application/yang"

        /// "application/yin+xml": YIN (YANG - XML)
        let yin_xml = MimeType "application/yin+xml"

        /// "application/zip": Zip Archive
        let zip = MimeType "application/zip"

    [<ReflectedDefinition>]
    module Audio =
        /// "audio/adpcm": Adaptive differential pulse-code modulation
        let adpcm = MimeType "audio/adpcm"

        /// "audio/basic": Sun Audio - Au file format
        let basic = MimeType "audio/basic"

        /// "audio/midi": MIDI - Musical Instrument Digital Interface
        let midi = MimeType "audio/midi"

        /// "audio/mp4": MPEG-4 Audio
        let mp4 = MimeType "audio/mp4"

        /// "audio/mpeg": MPEG Audio
        let mpeg = MimeType "audio/mpeg"

        /// "audio/ogg": Ogg Audio
        let ogg = MimeType "audio/ogg"

        /// "audio/vnd.dece.audio": DECE Audio
        let vnd_dece_audio = MimeType "audio/vnd.dece.audio"

        /// "audio/vnd.digital-winds": Digital Winds Music
        let vnd_digital_winds = MimeType "audio/vnd.digital-winds"

        /// "audio/vnd.dra": DRA Audio
        let vnd_dra = MimeType "audio/vnd.dra"

        /// "audio/vnd.dts": DTS Audio
        let vnd_dts = MimeType "audio/vnd.dts"

        /// "audio/vnd.dts.hd": DTS High Definition Audio
        let vnd_dts_hd = MimeType "audio/vnd.dts.hd"

        /// "audio/vnd.lucent.voice": Lucent Voice
        let vnd_lucent_voice = MimeType "audio/vnd.lucent.voice"

        /// "audio/vnd.ms-playready.media.pya": Microsoft PlayReady Ecosystem
        let vnd_ms_playready_media_pya = MimeType "audio/vnd.ms-playready.media.pya"

        /// "audio/vnd.nuera.ecelp4800": Nuera ECELP 4800
        let vnd_nuera_ecelp4800 = MimeType "audio/vnd.nuera.ecelp4800"

        /// "audio/vnd.nuera.ecelp7470": Nuera ECELP 7470
        let vnd_nuera_ecelp7470 = MimeType "audio/vnd.nuera.ecelp7470"

        /// "audio/vnd.nuera.ecelp9600": Nuera ECELP 9600
        let vnd_nuera_ecelp9600 = MimeType "audio/vnd.nuera.ecelp9600"

        /// "audio/vnd.rip": Hit'n'Mix
        let vnd_rip = MimeType "audio/vnd.rip"

        /// "audio/webm": Open Web Media Project - Audio
        let webm = MimeType "audio/webm"

        /// "audio/x-aac": Advanced Audio Coding (AAC)
        let x_aac = MimeType "audio/x-aac"

        /// "audio/x-aiff": Audio Interchange File Format
        let x_aiff = MimeType "audio/x-aiff"

        /// "audio/x-mpegurl": M3U (Multimedia Playlist)
        let x_mpegurl = MimeType "audio/x-mpegurl"

        /// "audio/x-ms-wax": Microsoft Windows Media Audio Redirector
        let x_ms_wax = MimeType "audio/x-ms-wax"

        /// "audio/x-ms-wma": Microsoft Windows Media Audio
        let x_ms_wma = MimeType "audio/x-ms-wma"

        /// "audio/x-pn-realaudio": Real Audio Sound
        let x_pn_realaudio = MimeType "audio/x-pn-realaudio"

        /// "audio/x-pn-realaudio-plugin": Real Audio Sound
        let x_pn_realaudio_plugin = MimeType "audio/x-pn-realaudio-plugin"

        /// "audio/x-wav": Waveform Audio File Format (WAV)
        let x_wav = MimeType "audio/x-wav"

    [<ReflectedDefinition>]
    module Chemical =
        /// "chemical/x-cdx": ChemDraw eXchange file
        let x_cdx = MimeType "chemical/x-cdx"

        /// "chemical/x-cif": Crystallographic Interchange Format
        let x_cif = MimeType "chemical/x-cif"

        /// "chemical/x-cmdf": CrystalMaker Data Format
        let x_cmdf = MimeType "chemical/x-cmdf"

        /// "chemical/x-cml": Chemical Markup Language
        let x_cml = MimeType "chemical/x-cml"

        /// "chemical/x-csml": Chemical Style Markup Language
        let x_csml = MimeType "chemical/x-csml"

        /// "chemical/x-xyz": XYZ File Format
        let x_xyz = MimeType "chemical/x-xyz"

    [<ReflectedDefinition>]
    module Image =
        /// "image/bmp": Bitmap Image File
        let bmp = MimeType "image/bmp"

        /// "image/cgm": Computer Graphics Metafile
        let cgm = MimeType "image/cgm"

        /// "image/g3fax": G3 Fax Image
        let g3fax = MimeType "image/g3fax"

        /// "image/gif": Graphics Interchange Format
        let gif = MimeType "image/gif"

        /// "image/ief": Image Exchange Format
        let ief = MimeType "image/ief"

        /// "image/jpeg": JPEG Image
        let jpeg = MimeType "image/jpeg"

        /// "image/ktx": OpenGL Textures (KTX)
        let ktx = MimeType "image/ktx"

        /// "image/png": Portable Network Graphics (PNG)
        let png = MimeType "image/png"

        /// "image/prs.btif": BTIF
        let prs_btif = MimeType "image/prs.btif"

        /// "image/svg+xml": Scalable Vector Graphics (SVG)
        let svg_xml = MimeType "image/svg+xml"

        /// "image/tiff": Tagged Image File Format
        let tiff = MimeType "image/tiff"

        /// "image/vnd.adobe.photoshop": Photoshop Document
        let vnd_adobe_photoshop = MimeType "image/vnd.adobe.photoshop"

        /// "image/vnd.dece.graphic": DECE Graphic
        let vnd_dece_graphic = MimeType "image/vnd.dece.graphic"

        /// "image/vnd.dvb.subtitle": Close Captioning - Subtitle
        let vnd_dvb_subtitle = MimeType "image/vnd.dvb.subtitle"

        /// "image/vnd.djvu": DjVu
        let vnd_djvu = MimeType "image/vnd.djvu"

        /// "image/vnd.dwg": DWG Drawing
        let vnd_dwg = MimeType "image/vnd.dwg"

        /// "image/vnd.dxf": AutoCAD DXF
        let vnd_dxf = MimeType "image/vnd.dxf"

        /// "image/vnd.fastbidsheet": FastBid Sheet
        let vnd_fastbidsheet = MimeType "image/vnd.fastbidsheet"

        /// "image/vnd.fpx": FlashPix
        let vnd_fpx = MimeType "image/vnd.fpx"

        /// "image/vnd.fst": FAST Search & Transfer ASA
        let vnd_fst = MimeType "image/vnd.fst"

        /// "image/vnd.fujixerox.edmics-mmr": EDMICS 2000
        let vnd_fujixerox_edmics_mmr = MimeType "image/vnd.fujixerox.edmics-mmr"

        /// "image/vnd.fujixerox.edmics-rlc": EDMICS 2000
        let vnd_fujixerox_edmics_rlc = MimeType "image/vnd.fujixerox.edmics-rlc"

        /// "image/vnd.ms-modi": Microsoft Document Imaging Format
        let vnd_ms_modi = MimeType "image/vnd.ms-modi"

        /// "image/vnd.net-fpx": FlashPix
        let vnd_net_fpx = MimeType "image/vnd.net-fpx"

        /// "image/vnd.wap.wbmp": WAP Bitamp (WBMP)
        let vnd_wap_wbmp = MimeType "image/vnd.wap.wbmp"

        /// "image/vnd.xiff": eXtended Image File Format (XIFF)
        let vnd_xiff = MimeType "image/vnd.xiff"

        /// "image/webp": WebP Image
        let webp = MimeType "image/webp"

        /// "image/x-cmu-raster": CMU Image
        let x_cmu_raster = MimeType "image/x-cmu-raster"

        /// "image/x-cmx": Corel Metafile Exchange (CMX)
        let x_cmx = MimeType "image/x-cmx"

        /// "image/x-freehand": FreeHand MX
        let x_freehand = MimeType "image/x-freehand"

        /// "image/x-icon": Icon Image
        let x_icon = MimeType "image/x-icon"

        /// "image/x-pcx": PCX Image
        let x_pcx = MimeType "image/x-pcx"

        /// "image/x-pict": PICT Image
        let x_pict = MimeType "image/x-pict"

        /// "image/x-portable-anymap": Portable Anymap Image
        let x_portable_anymap = MimeType "image/x-portable-anymap"

        /// "image/x-portable-bitmap": Portable Bitmap Format
        let x_portable_bitmap = MimeType "image/x-portable-bitmap"

        /// "image/x-portable-graymap": Portable Graymap Format
        let x_portable_graymap = MimeType "image/x-portable-graymap"

        /// "image/x-portable-pixmap": Portable Pixmap Format
        let x_portable_pixmap = MimeType "image/x-portable-pixmap"

        /// "image/x-rgb": Silicon Graphics RGB Bitmap
        let x_rgb = MimeType "image/x-rgb"

        /// "image/x-xbitmap": X BitMap
        let x_xbitmap = MimeType "image/x-xbitmap"

        /// "image/x-xpixmap": X PixMap
        let x_xpixmap = MimeType "image/x-xpixmap"

        /// "image/x-xwindowdump": X Window Dump
        let x_xwindowdump = MimeType "image/x-xwindowdump"

    [<ReflectedDefinition>]
    module Message =
        /// "message/rfc822": Email Message
        let rfc822 = MimeType "message/rfc822"

    [<ReflectedDefinition>]
    module Model =
        /// "model/iges": Initial Graphics Exchange Specification (IGES)
        let iges = MimeType "model/iges"

        /// "model/mesh": Mesh Data Type
        let mesh = MimeType "model/mesh"

        /// "model/vnd.collada+xml": COLLADA
        let vnd_collada_xml = MimeType "model/vnd.collada+xml"

        /// "model/vnd.dwf": Autodesk Design Web Format (DWF)
        let vnd_dwf = MimeType "model/vnd.dwf"

        /// "model/vnd.gdl": Geometric Description Language (GDL)
        let vnd_gdl = MimeType "model/vnd.gdl"

        /// "model/vnd.gtw": Gen-Trix Studio
        let vnd_gtw = MimeType "model/vnd.gtw"

        /// "model/vnd.mts": Virtue MTS
        let vnd_mts = MimeType "model/vnd.mts"

        /// "model/vnd.vtu": Virtue VTU
        let vnd_vtu = MimeType "model/vnd.vtu"

        /// "model/vrml": Virtual Reality Modeling Language
        let vrml = MimeType "model/vrml"

    [<ReflectedDefinition>]
    module Text =
        /// "text/calendar": iCalendar
        let calendar = MimeType "text/calendar"

        /// "text/css": Cascading Style Sheets (CSS)
        let css = MimeType "text/css"

        /// "text/csv": Comma-Seperated Values
        let csv = MimeType "text/csv"

        /// "text/html": HyperText Markup Language (HTML)
        let html = MimeType "text/html"

        /// "text/n3": Notation3
        let n3 = MimeType "text/n3"

        /// "text/plain": Text File
        let plain = MimeType "text/plain"

        /// "text/prs.lines.tag": PRS Lines Tag
        let prs_lines_tag = MimeType "text/prs.lines.tag"

        /// "text/richtext": Rich Text Format (RTF)
        let richtext = MimeType "text/richtext"

        /// "text/sgml": Standard Generalized Markup Language (SGML)
        let sgml = MimeType "text/sgml"

        /// "text/tab-separated-values": Tab Seperated Values
        let tab_separated_values = MimeType "text/tab-separated-values"

        /// "text/troff": troff
        let troff = MimeType "text/troff"

        /// "text/turtle": Turtle (Terse RDF Triple Language)
        let turtle = MimeType "text/turtle"

        /// "text/uri-list": URI Resolution Services
        let uri_list = MimeType "text/uri-list"

        /// "text/vnd.curl": Curl - Applet
        let vnd_curl = MimeType "text/vnd.curl"

        /// "text/vnd.curl.dcurl": Curl - Detached Applet
        let vnd_curl_dcurl = MimeType "text/vnd.curl.dcurl"

        /// "text/vnd.curl.scurl": Curl - Source Code
        let vnd_curl_scurl = MimeType "text/vnd.curl.scurl"

        /// "text/vnd.curl.mcurl": Curl - Manifest File
        let vnd_curl_mcurl = MimeType "text/vnd.curl.mcurl"

        /// "text/vnd.fly": mod_fly / fly.cgi
        let vnd_fly = MimeType "text/vnd.fly"

        /// "text/vnd.fmi.flexstor": FLEXSTOR
        let vnd_fmi_flexstor = MimeType "text/vnd.fmi.flexstor"

        /// "text/vnd.graphviz": Graphviz
        let vnd_graphviz = MimeType "text/vnd.graphviz"

        /// "text/vnd.in3d.3dml": In3D - 3DML
        let vnd_in3d_3dml = MimeType "text/vnd.in3d.3dml"

        /// "text/vnd.in3d.spot": In3D - 3DML
        let vnd_in3d_spot = MimeType "text/vnd.in3d.spot"

        /// "text/vnd.sun.j2me.app-descriptor": J2ME App Descriptor
        let vnd_sun_j2me_app_descriptor = MimeType "text/vnd.sun.j2me.app-descriptor"

        /// "text/vnd.wap.wml": Wireless Markup Language (WML)
        let vnd_wap_wml = MimeType "text/vnd.wap.wml"

        /// "text/vnd.wap.wmlscript": Wireless Markup Language Script (WMLScript)
        let vnd_wap_wmlscript = MimeType "text/vnd.wap.wmlscript"

        /// "text/x-asm": Assembler Source File
        let x_asm = MimeType "text/x-asm"

        /// "text/x-c": C Source File
        let x_c = MimeType "text/x-c"

        /// "text/x-fortran": Fortran Source File
        let x_fortran = MimeType "text/x-fortran"

        /// "text/x-pascal": Pascal Source File
        let x_pascal = MimeType "text/x-pascal"

        /// "text/x-java-source,java": Java Source File
        let x_java_source_java = MimeType "text/x-java-source,java"

        /// "text/x-setext": Setext
        let x_setext = MimeType "text/x-setext"

        /// "text/x-uuencode": UUEncode
        let x_uuencode = MimeType "text/x-uuencode"

        /// "text/x-vcalendar": vCalendar
        let x_vcalendar = MimeType "text/x-vcalendar"

        /// "text/x-vcard": vCard
        let x_vcard = MimeType "text/x-vcard"

        /// "text/plain-bas": BAS Partitur Format
        let plain_bas = MimeType "text/plain-bas"

        /// "text/yaml": YAML Ain't Markup Language / Yet Another Markup Language
        let yaml = MimeType "text/yaml"

    [<ReflectedDefinition>]
    module Video =
        /// "video/3gpp": 3GP
        let _3gpp = MimeType "video/3gpp"

        /// "video/3gpp2": 3GP2
        let _3gpp2 = MimeType "video/3gpp2"

        /// "video/h261": H.261
        let h261 = MimeType "video/h261"

        /// "video/h263": H.263
        let h263 = MimeType "video/h263"

        /// "video/h264": H.264
        let h264 = MimeType "video/h264"

        /// "video/jpeg": JPGVideo
        let jpeg = MimeType "video/jpeg"

        /// "video/jpm": JPEG 2000 Compound Image File Format
        let jpm = MimeType "video/jpm"

        /// "video/mj2": Motion JPEG 2000
        let mj2 = MimeType "video/mj2"

        /// "video/mp4": MPEG-4 Video
        let mp4 = MimeType "video/mp4"

        /// "video/mpeg": MPEG Video
        let mpeg = MimeType "video/mpeg"

        /// "video/ogg": Ogg Video
        let ogg = MimeType "video/ogg"

        /// "video/quicktime": Quicktime Video
        let quicktime = MimeType "video/quicktime"

        /// "video/vnd.dece.hd": DECE High Definition Video
        let vnd_dece_hd = MimeType "video/vnd.dece.hd"

        /// "video/vnd.dece.mobile": DECE Mobile Video
        let vnd_dece_mobile = MimeType "video/vnd.dece.mobile"

        /// "video/vnd.dece.pd": DECE PD Video
        let vnd_dece_pd = MimeType "video/vnd.dece.pd"

        /// "video/vnd.dece.sd": DECE SD Video
        let vnd_dece_sd = MimeType "video/vnd.dece.sd"

        /// "video/vnd.dece.video": DECE Video
        let vnd_dece_video = MimeType "video/vnd.dece.video"

        /// "video/vnd.fvt": FAST Search & Transfer ASA
        let vnd_fvt = MimeType "video/vnd.fvt"

        /// "video/vnd.mpegurl": MPEG Url
        let vnd_mpegurl = MimeType "video/vnd.mpegurl"

        /// "video/vnd.ms-playready.media.pyv": Microsoft PlayReady Ecosystem Video
        let vnd_ms_playready_media_pyv = MimeType "video/vnd.ms-playready.media.pyv"

        /// "video/vnd.uvvu.mp4": DECE MP4
        let vnd_uvvu_mp4 = MimeType "video/vnd.uvvu.mp4"

        /// "video/vnd.vivo": Vivo
        let vnd_vivo = MimeType "video/vnd.vivo"

        /// "video/webm": Open Web Media Project - Video
        let webm = MimeType "video/webm"

        /// "video/x-f4v": Flash Video
        let x_f4v = MimeType "video/x-f4v"

        /// "video/x-fli": FLI/FLC Animation Format
        let x_fli = MimeType "video/x-fli"

        /// "video/x-flv": Flash Video
        let x_flv = MimeType "video/x-flv"

        /// "video/x-m4v": M4v
        let x_m4v = MimeType "video/x-m4v"

        /// "video/x-ms-asf": Microsoft Advanced Systems Format (ASF)
        let x_ms_asf = MimeType "video/x-ms-asf"

        /// "video/x-ms-wm": Microsoft Windows Media
        let x_ms_wm = MimeType "video/x-ms-wm"

        /// "video/x-ms-wmv": Microsoft Windows Media Video
        let x_ms_wmv = MimeType "video/x-ms-wmv"

        /// "video/x-ms-wmx": Microsoft Windows Media Audio/Video Playlist
        let x_ms_wmx = MimeType "video/x-ms-wmx"

        /// "video/x-ms-wvx": Microsoft Windows Media Video Playlist
        let x_ms_wvx = MimeType "video/x-ms-wvx"

        /// "video/x-msvideo": Audio Video Interleave (AVI)
        let x_msvideo = MimeType "video/x-msvideo"

        /// "video/x-sgi-movie": SGI Movie
        let x_sgi_movie = MimeType "video/x-sgi-movie"

    [<ReflectedDefinition>]
    module X_conference =
        /// "x-conference/x-cooltalk": CoolTalk
        let x_cooltalk = MimeType "x-conference/x-cooltalk"
