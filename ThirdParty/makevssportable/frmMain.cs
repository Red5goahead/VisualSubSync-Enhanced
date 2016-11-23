using System;
using System.IO;
using System.Text;
using System.Windows.Forms;
using ICSharpCode.SharpZipLib.Zip;
using makevssportable.Properties;

/*
 * Copyright 2012 rock3r
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License. 
 */


namespace makevssportable
{
    public partial class frmMain : Form
    {
        public frmMain()
        {
            InitializeComponent();
        }

        private void btnBrowse_Click(object sender, EventArgs e)
        {
            if (dlgBrowseFolders.ShowDialog(this) == DialogResult.OK)
            {
                txtOutputDir.Text = dlgBrowseFolders.SelectedPath;
            }
        }

        private void btnCreate_Click(object sender, EventArgs e)
        {
            // Check output path
            string outputPath = txtOutputDir.Text, startupDir = Application.StartupPath;
            if (!Directory.Exists(outputPath))
            {
                MessageBox.Show(this,
                                "The selected output path isn't valid or doesn't exist. Enter a valid output path and retry.",
                                Application.ProductName, MessageBoxButtons.OK, MessageBoxIcon.Warning);
                txtOutputDir.Focus();
                return;
            }

            if((new DirectoryInfo(outputPath).GetFiles().Length > 0))
            {
                if (MessageBox.Show(this,
                                "The selected output path contains some files. If you continue they will be erased. Do you still want to continue?",
                                Application.ProductName, MessageBoxButtons.YesNo, MessageBoxIcon.Warning) == DialogResult.No)
                    return;
            }
            
            // Prepare the form
            Enabled = false;
            Cursor = Cursors.WaitCursor;

            // Copy stuff from VSS %installdir%
            try
            {
                // Cleanup the output directory
                cleanupDir(outputPath);

                // Copy program files from the install directory
                copyFilesFromInstallDir(startupDir, outputPath);

                // Now copy stuff from the %appdata% storage, if it exists and the user told us to.
                string appDataPath = Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.ApplicationData),
                                                  "VisualSubSync");

                if (copySettings(startupDir, appDataPath, outputPath)) return;

                MessageBox.Show(this, "Operation successfully completed.", Application.ProductName, MessageBoxButtons.OK, MessageBoxIcon.Information);
            }
            catch (Exception ex)
            {
                System.Diagnostics.Debug.Print("ERROR: {0}\n{1}", ex.Message, ex.StackTrace);
                MessageBox.Show(this,
                                "Unable to copy files to the output dir.\nThe following error occoured:\n" + ex.Message,
                                Application.ProductName, MessageBoxButtons.OK, MessageBoxIcon.Warning);
            }
            finally
            {
                Enabled = true;
                Cursor = Cursors.Default;
            }
        }

        private static void cleanupDir(string outputPath)
        {
            DirectoryInfo dir = new DirectoryInfo(outputPath);

            foreach (FileInfo fi in dir.GetFiles())
            {
                fi.IsReadOnly = false;
                fi.Delete();
            }

            foreach (DirectoryInfo di in dir.GetDirectories())
            {
                cleanupDir(di.FullName);
                di.Delete();
            }
        }

        private bool copySettings(string installDir, string appDataPath, string outputPath)
        {
            if (Settings.Default.UseDefaults)
            {
                copySettingsFromDeployDir(installDir, outputPath);
            }
            else if (!Directory.Exists(appDataPath))
            {
                if (
                    MessageBox.Show(this,
                                    "Unable to find the user preferences. Continue anyway? The default values will be used instead.",
                                    Application.ProductName, MessageBoxButtons.YesNo, MessageBoxIcon.Warning) ==
                    DialogResult.No)
                {
                    return true;
                }
                copySettingsFromDeployDir(installDir, outputPath);
            }
            else
            {
                copySettingsFromAppData(appDataPath, outputPath, installDir);
            }

            return false;
        }

        private void copySettingsFromAppData(string appDataPath, string outputPath, string installDir)
        {
            // If we're missing the ini file or the dictionaries, first copy the 
            // default ones, we'll overwrite them immediately if they exist in %appdata%
            if (Directory.GetFiles(appDataPath, "VisualSubSync.ini").Length == 0 || Directory.GetDirectories(appDataPath, "dict").Length == 0)
                copySettingsFromDeployDir(installDir, outputPath);
            
            // Try to copy user preferences from %appdata%\VisualSubSync
            FileInfo[] files = (new DirectoryInfo(appDataPath)).GetFiles("*", SearchOption.AllDirectories);
            foreach (FileInfo file in files)
            {
                string relativePath = getRelativePath(appDataPath, file.FullName);

                // Create the destination path if needed
                string destFileName = Path.Combine(outputPath, relativePath);
                string directoryName = Path.GetDirectoryName(destFileName);
                if (!String.IsNullOrEmpty(directoryName) && !Directory.Exists(directoryName))
                {
                    Directory.CreateDirectory(directoryName);
                }

                file.CopyTo(destFileName, true);
            }
        }

        private static void copySettingsFromDeployDir(string installDir, string outputPath)
        {
            // Try to copy the default settings from the deploy dir
            FileInfo[] files = (new DirectoryInfo(Path.Combine(installDir, "deploy"))).GetFiles("*.zip");
            foreach (FileInfo file in files)
            {
                string relativePath = string.Empty;

                // Set relative path where needed for known files
                switch (file.Name.ToLower())
                {
                    case "perso.zip":
                        //relativePath = "dict";
                        break;
                }

                UnzipFile(file.FullName, Path.Combine(outputPath, relativePath));
            }
        }

        public static void UnzipFile(string filename, string outPath)
        {
            using (var s = new ZipInputStream(File.OpenRead(filename)))
            {
                ZipEntry entry;
                while ((entry = s.GetNextEntry()) != null)
                {
                    string path = Path.GetDirectoryName(entry.Name);
                    string name = Path.GetFileName(entry.Name);

                    // Create output directory if needed
                    if (!string.IsNullOrEmpty(path))
                    {
                        Directory.CreateDirectory(Path.Combine(outPath, path));
                    }

                    if (name != String.Empty)
                    {
                        using (FileStream streamWriter = File.Create(Path.Combine(Path.Combine(outPath, path), name)))
                        {
                            var data = new byte[32768];
                            while (true)
                            {
                                int size = s.Read(data, 0, data.Length);
                                if (size > 0)
                                {
                                    streamWriter.Write(data, 0, size);
                                }
                                else
                                {
                                    streamWriter.Close();
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }

        protected void copyFilesFromInstallDir(string installDir, string outputPath)
        {
            // Try to move stuff around (assuming this tool is run from VSS folder)
            FileInfo[] files = (new DirectoryInfo(installDir)).GetFiles("*", SearchOption.AllDirectories);
            foreach (FileInfo file in files)
            {
                // Skip the zip files in the "deploy" dir
                string relativePath = getRelativePath(installDir, file.FullName);
                if (file.Extension.Equals(".zip", StringComparison.OrdinalIgnoreCase) &&
                    relativePath.StartsWith(@"deploy\", StringComparison.OrdinalIgnoreCase))
                {
                    continue;
                }

                // Create the destination path if needed
                string destFileName = Path.Combine(outputPath, relativePath);
                string directoryName = Path.GetDirectoryName(destFileName);
                if (!String.IsNullOrEmpty(directoryName) && !Directory.Exists(directoryName))
                {
                    Directory.CreateDirectory(directoryName);
                }

                file.CopyTo(destFileName, true);
            }
        }

        private void frmMain_Load(object sender, EventArgs e)
        {
            Text = Application.ProductName;

            // Check startup path
            var startupDir = new DirectoryInfo(Application.StartupPath);
            if (startupDir.GetFiles("VisualSubSync.exe", SearchOption.TopDirectoryOnly).Length == 0)
            {
                MessageBox.Show(this,
                                "VisualSubSync not found in the current directory. Please make sure you're running this tool from VisualSubSync install directory.",
                                Application.ProductName, MessageBoxButtons.OK, MessageBoxIcon.Warning);
                Application.Exit();
            }
        }

        private string getRelativePath(string basePath, string otherPath)
        {
            string[] lStartPathParts =
                Path.GetFullPath(basePath).Trim(Path.DirectorySeparatorChar).Split(Path.DirectorySeparatorChar);
            string[] lDestinationPathParts = otherPath.Split(Path.DirectorySeparatorChar);

            int lSameCounter = 0;
            while ((lSameCounter < lStartPathParts.Length) && (lSameCounter < lDestinationPathParts.Length) &&
                   lStartPathParts[lSameCounter].Equals(lDestinationPathParts[lSameCounter],
                                                        StringComparison.InvariantCultureIgnoreCase))
            {
                lSameCounter++;
            }

            if (lSameCounter == 0)
            {
                return otherPath; // There is no relative link.
            }

            var lBuilder = new StringBuilder();
            for (int i = lSameCounter; i < lStartPathParts.Length; i++)
            {
                lBuilder.Append(".." + Path.DirectorySeparatorChar);
            }

            for (int i = lSameCounter; i < lDestinationPathParts.Length; i++)
            {
                lBuilder.Append(lDestinationPathParts[i] + Path.DirectorySeparatorChar);
            }

            lBuilder.Length--;

            return lBuilder.ToString();
        }

        private void btnAbout_Click(object sender, EventArgs e)
        {
            var about = new frmAbout();
            about.ShowDialog(this);
        }
    }
}