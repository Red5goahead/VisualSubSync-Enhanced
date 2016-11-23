namespace makevssportable
{
    partial class frmMain
    {
        /// <summary>
        /// Variabile di progettazione necessaria.
        /// </summary>
        private System.ComponentModel.IContainer components = null;

        /// <summary>
        /// Liberare le risorse in uso.
        /// </summary>
        /// <param name="disposing">ha valore true se le risorse gestite devono essere eliminate, false in caso contrario.</param>
        protected override void Dispose(bool disposing)
        {
            if (disposing && (components != null))
            {
                components.Dispose();
            }
            base.Dispose(disposing);
        }

        #region Codice generato da Progettazione Windows Form

        /// <summary>
        /// Metodo necessario per il supporto della finestra di progettazione. Non modificare
        /// il contenuto del metodo con l'editor di codice.
        /// </summary>
        private void InitializeComponent()
        {
            System.ComponentModel.ComponentResourceManager resources = new System.ComponentModel.ComponentResourceManager(typeof(frmMain));
            this.pnlFooter = new System.Windows.Forms.Panel();
            this.btnCreate = new System.Windows.Forms.Button();
            this.lblOptions = new System.Windows.Forms.Label();
            this.lblUseDefaultsInfo = new System.Windows.Forms.Label();
            this.lblOutput = new System.Windows.Forms.Label();
            this.btnBrowse = new System.Windows.Forms.Button();
            this.dlgBrowseFolders = new System.Windows.Forms.FolderBrowserDialog();
            this.txtOutputDir = new System.Windows.Forms.TextBox();
            this.chkUseDefaults = new System.Windows.Forms.CheckBox();
            this.btnAbout = new System.Windows.Forms.Button();
            this.pnlFooter.SuspendLayout();
            this.SuspendLayout();
            // 
            // pnlFooter
            // 
            this.pnlFooter.BackColor = System.Drawing.SystemColors.Control;
            this.pnlFooter.Controls.Add(this.btnAbout);
            this.pnlFooter.Controls.Add(this.btnCreate);
            this.pnlFooter.Dock = System.Windows.Forms.DockStyle.Bottom;
            this.pnlFooter.Location = new System.Drawing.Point(0, 204);
            this.pnlFooter.Name = "pnlFooter";
            this.pnlFooter.Size = new System.Drawing.Size(430, 49);
            this.pnlFooter.TabIndex = 0;
            // 
            // btnCreate
            // 
            this.btnCreate.Anchor = ((System.Windows.Forms.AnchorStyles)((System.Windows.Forms.AnchorStyles.Bottom | System.Windows.Forms.AnchorStyles.Right)));
            this.btnCreate.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.btnCreate.Location = new System.Drawing.Point(344, 14);
            this.btnCreate.Name = "btnCreate";
            this.btnCreate.Size = new System.Drawing.Size(74, 23);
            this.btnCreate.TabIndex = 7;
            this.btnCreate.Text = "&Create";
            this.btnCreate.UseVisualStyleBackColor = true;
            this.btnCreate.Click += new System.EventHandler(this.btnCreate_Click);
            // 
            // lblOptions
            // 
            this.lblOptions.AutoSize = true;
            this.lblOptions.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.lblOptions.Font = new System.Drawing.Font("Segoe UI Light", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblOptions.ForeColor = System.Drawing.SystemColors.HotTrack;
            this.lblOptions.Location = new System.Drawing.Point(15, 9);
            this.lblOptions.Name = "lblOptions";
            this.lblOptions.Size = new System.Drawing.Size(75, 25);
            this.lblOptions.TabIndex = 1;
            this.lblOptions.Text = "Options";
            // 
            // lblUseDefaultsInfo
            // 
            this.lblUseDefaultsInfo.ForeColor = System.Drawing.SystemColors.GrayText;
            this.lblUseDefaultsInfo.Location = new System.Drawing.Point(31, 67);
            this.lblUseDefaultsInfo.Name = "lblUseDefaultsInfo";
            this.lblUseDefaultsInfo.Size = new System.Drawing.Size(376, 36);
            this.lblUseDefaultsInfo.TabIndex = 3;
            this.lblUseDefaultsInfo.Text = "Check this options to avoid using the user custom VisualSubSync settings, and use" +
    " the default settings instead.";
            // 
            // lblOutput
            // 
            this.lblOutput.AutoSize = true;
            this.lblOutput.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.lblOutput.Font = new System.Drawing.Font("Segoe UI Light", 14.25F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((byte)(0)));
            this.lblOutput.ForeColor = System.Drawing.SystemColors.HotTrack;
            this.lblOutput.Location = new System.Drawing.Point(15, 120);
            this.lblOutput.Name = "lblOutput";
            this.lblOutput.Size = new System.Drawing.Size(121, 25);
            this.lblOutput.TabIndex = 4;
            this.lblOutput.Text = "Output folder";
            // 
            // btnBrowse
            // 
            this.btnBrowse.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.btnBrowse.Location = new System.Drawing.Point(333, 152);
            this.btnBrowse.Name = "btnBrowse";
            this.btnBrowse.Size = new System.Drawing.Size(74, 23);
            this.btnBrowse.TabIndex = 6;
            this.btnBrowse.Text = "&Browse...";
            this.btnBrowse.UseVisualStyleBackColor = true;
            this.btnBrowse.Click += new System.EventHandler(this.btnBrowse_Click);
            // 
            // dlgBrowseFolders
            // 
            this.dlgBrowseFolders.Description = "Select the folder where to put the portable VisualSubSync copy. Choose an empty f" +
    "older!";
            // 
            // txtOutputDir
            // 
            this.txtOutputDir.AutoCompleteMode = System.Windows.Forms.AutoCompleteMode.Suggest;
            this.txtOutputDir.AutoCompleteSource = System.Windows.Forms.AutoCompleteSource.FileSystemDirectories;
            this.txtOutputDir.DataBindings.Add(new System.Windows.Forms.Binding("Text", global::makevssportable.Properties.Settings.Default, "lastOutDir", true, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged));
            this.txtOutputDir.Location = new System.Drawing.Point(17, 152);
            this.txtOutputDir.Name = "txtOutputDir";
            this.txtOutputDir.Size = new System.Drawing.Size(310, 23);
            this.txtOutputDir.TabIndex = 5;
            this.txtOutputDir.Text = global::makevssportable.Properties.Settings.Default.lastOutDir;
            // 
            // chkUseDefaults
            // 
            this.chkUseDefaults.AutoSize = true;
            this.chkUseDefaults.Checked = global::makevssportable.Properties.Settings.Default.UseDefaults;
            this.chkUseDefaults.DataBindings.Add(new System.Windows.Forms.Binding("Checked", global::makevssportable.Properties.Settings.Default, "UseDefaults", true, System.Windows.Forms.DataSourceUpdateMode.OnPropertyChanged));
            this.chkUseDefaults.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.chkUseDefaults.Location = new System.Drawing.Point(17, 43);
            this.chkUseDefaults.Name = "chkUseDefaults";
            this.chkUseDefaults.Size = new System.Drawing.Size(135, 20);
            this.chkUseDefaults.TabIndex = 2;
            this.chkUseDefaults.Text = "Use &default settings";
            this.chkUseDefaults.UseVisualStyleBackColor = true;
            // 
            // btnAbout
            // 
            this.btnAbout.FlatStyle = System.Windows.Forms.FlatStyle.System;
            this.btnAbout.Location = new System.Drawing.Point(12, 14);
            this.btnAbout.Name = "btnAbout";
            this.btnAbout.Size = new System.Drawing.Size(75, 23);
            this.btnAbout.TabIndex = 8;
            this.btnAbout.Text = "&About...";
            this.btnAbout.UseVisualStyleBackColor = true;
            this.btnAbout.Click += new System.EventHandler(this.btnAbout_Click);
            // 
            // frmMain
            // 
            this.AcceptButton = this.btnCreate;
            this.AutoScaleDimensions = new System.Drawing.SizeF(7F, 15F);
            this.AutoScaleMode = System.Windows.Forms.AutoScaleMode.Font;
            this.BackColor = System.Drawing.SystemColors.Window;
            this.ClientSize = new System.Drawing.Size(430, 253);
            this.Controls.Add(this.btnBrowse);
            this.Controls.Add(this.txtOutputDir);
            this.Controls.Add(this.lblOutput);
            this.Controls.Add(this.lblUseDefaultsInfo);
            this.Controls.Add(this.chkUseDefaults);
            this.Controls.Add(this.lblOptions);
            this.Controls.Add(this.pnlFooter);
            this.Font = new System.Drawing.Font("Segoe UI", 9F);
            this.ForeColor = System.Drawing.SystemColors.ControlText;
            this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedSingle;
            this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
            this.MaximizeBox = false;
            this.Name = "frmMain";
            this.Text = "Portable VisualSubSync Tool";
            this.Load += new System.EventHandler(this.frmMain_Load);
            this.pnlFooter.ResumeLayout(false);
            this.ResumeLayout(false);
            this.PerformLayout();

        }

        #endregion

        private System.Windows.Forms.Panel pnlFooter;
        private System.Windows.Forms.Label lblOptions;
        private System.Windows.Forms.CheckBox chkUseDefaults;
        private System.Windows.Forms.Label lblUseDefaultsInfo;
        private System.Windows.Forms.Label lblOutput;
        private System.Windows.Forms.TextBox txtOutputDir;
        private System.Windows.Forms.Button btnBrowse;
        private System.Windows.Forms.Button btnCreate;
        private System.Windows.Forms.FolderBrowserDialog dlgBrowseFolders;
        private System.Windows.Forms.Button btnAbout;
    }
}

