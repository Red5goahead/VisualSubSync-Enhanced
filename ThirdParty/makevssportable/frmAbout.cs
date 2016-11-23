using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Text;
using System.Windows.Forms;

namespace makevssportable
{
    public partial class frmAbout : Form
    {
        public frmAbout()
        {
            InitializeComponent();
        }

        private void frmAbout_Load(object sender, EventArgs e)
        {
            Text = string.Format("About {0}", Application.ProductName);
            lblAppName.Text = Application.ProductName;
            lblVersion.Text = string.Format("Version {0}", Application.ProductVersion);
        }
    }
}
