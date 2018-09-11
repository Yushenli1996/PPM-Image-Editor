using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using Microsoft.FSharp.Collections;

namespace PPMImageEditor
{
  public partial class Form1 : Form
  {
    //
    // Current image being displayed:
    //
    private PixelMap CurrentImage;

    public Form1()
    {
      InitializeComponent();
    }

    // 
    // Initialize window just before it appears:
    //
    private void Form1_Load(object sender, EventArgs e)
    {
      this.radioFitImage.Checked = true;
    }

    // 
    // Exit:
    private void cmdExit_Click(object sender, EventArgs e)
    {
      this.Close();
    }

    
    //
    // Open:
    //
    private void cmdOpen_Click(object sender, EventArgs e)
    {
      openFileDialog1.Filter = "PPM Files (*.ppm)|*.ppm|All files (*.*)|*.*";
      openFileDialog1.FileName = "";
      openFileDialog1.CheckFileExists = true;
      openFileDialog1.InitialDirectory = System.Environment.CurrentDirectory;

      DialogResult dr = openFileDialog1.ShowDialog();

      if (dr == System.Windows.Forms.DialogResult.OK)
      {
        string filepath = openFileDialog1.FileName;

        CurrentImage = new PixelMap(filepath);
        picImage.Image = CurrentImage.BitMap;

        // enable the other buttons so user can manipulate image:
        cmdFS1.Enabled = true;
        cmdSaveAs.Enabled = true;
        button1.Enabled = true;
        button2.Enabled = true;
        button3.Enabled = true;
        button4.Enabled = true;
        button5.Enabled = true;
      }
      else
      {
        MessageBox.Show("Open was canceled...");
      }
    }//cmdOpen


    //
    // Test F#:
    //
    private void cmdFS1_Click(object sender, EventArgs e)
    {
      if (CurrentImage == null)  // sanity check: make sure we have an image to manipulate
        return;

      //
      // Example of calling library, which then dumps data about the given image
      // to Visual Studio's "Output" window.  Note you must run with debugging (F5)
      // in order to see the output.
      //
      //PPMImageLibrary.DebugOutput(
      //  CurrentImage.Header.Width,
      //  CurrentImage.Header.Height,
      //  CurrentImage.Header.Depth,
      //  CurrentImage.ImageListData
      //);

      //
      // we have an image, perform transformation and display new result:
      //
      FSharpList<FSharpList<Tuple<int,int,int>>> newImageList;

      newImageList = PPMImageLibrary.TransformFirstThreeRows(
        CurrentImage.Header.Width,
        CurrentImage.Header.Height,
        CurrentImage.Header.Depth,
        CurrentImage.ImageListData
      );

      //
      // create a new PixelMap here on the client-side, which creates a new bitmap
      // we then display to the user:
      //
      CurrentImage = new PixelMap(newImageList);
      picImage.Image = CurrentImage.BitMap;
    }//cmdFS1


    //
    // Save as:
    //
    private void cmdSaveAs_Click(object sender, EventArgs e)
    {
      if (CurrentImage == null)  // sanity check: make sure we have an image to save
        return;

      saveFileDialog1.Filter = "PPM Files (*.ppm)|*.ppm";
      saveFileDialog1.DefaultExt = "ppm";
      saveFileDialog1.FileName = "";
      saveFileDialog1.InitialDirectory = System.Environment.CurrentDirectory;

      DialogResult dr = saveFileDialog1.ShowDialog();

      if (dr == System.Windows.Forms.DialogResult.OK)
      {
        string filepath = saveFileDialog1.FileName;

        bool written = PPMImageLibrary.WriteP3Image(
          filepath,
          CurrentImage.Header.Width,
          CurrentImage.Header.Height,
          CurrentImage.Header.Depth,
          CurrentImage.ImageListData
        );

        if (!written)
          MessageBox.Show("Write failed?!");
      }
      else
      {
        MessageBox.Show("Save As was canceled...");
      }
    }//cmdSaveAs


    private void button1_Click(object sender, EventArgs e)
    {
      if (CurrentImage == null)  // sanity check: make sure we have an image to manipulate
        return;

      //
      // Example of calling library, which then dumps data about the given image
      // to Visual Studio's "Output" window.  Note you must run with debugging (F5)
      // in order to see the output.
      //
      //PPMImageLibrary.DebugOutput(
      //  CurrentImage.Header.Width,
      //  CurrentImage.Header.Height, 
      //  CurrentImage.Header.Depth,
      //  CurrentImage.ImageListData
      //);

      //
      // we have an image, perform transformation and display new result:
      //
      FSharpList<FSharpList<Tuple<int, int, int>>> newImageList;

      newImageList = PPMImageLibrary.Grayscale(
        CurrentImage.Header.Width,
        CurrentImage.Header.Height,
        CurrentImage.Header.Depth,
        CurrentImage.ImageListData
      );

      //
      // create a new PixelMap here on the client-side, which creates a new bitmap
      // we then display to the user:
      //
      CurrentImage = new PixelMap(newImageList);
      picImage.Image = CurrentImage.BitMap;
    }


    private void button2_Click(object sender, EventArgs e)
    {
      if (CurrentImage == null)  // sanity check: make sure we have an image to manipulate
        return;

      //
      // Example of calling library, which then dumps data about the given image
      // to Visual Studio's "Output" window.  Note you must run with debugging (F5)
      // in order to see the output.
      //
      //PPMImageLibrary.DebugOutput(
      //  CurrentImage.Header.Width,
      //  CurrentImage.Header.Height, 
      //  CurrentImage.Header.Depth,
      //  CurrentImage.ImageListData
      //);

      //
      // we have an image, perform transformation and display new result:
      //
      FSharpList<FSharpList<Tuple<int, int, int>>> newImageList;

      int threshold;
      bool success = System.Int32.TryParse(txtThreshold.Text, out threshold);

      if (!success)
      {
        threshold = 50;
        txtFactor.Text = threshold.ToString();
      }

      newImageList = PPMImageLibrary.Threshold(
        CurrentImage.Header.Width,
        CurrentImage.Header.Height,
        CurrentImage.Header.Depth,
        CurrentImage.ImageListData,
        threshold
      );

      //
      // create a new PixelMap here on the client-side, which creates a new bitmap
      // we then display to the user:
      //
      CurrentImage = new PixelMap(newImageList);
      picImage.Image = CurrentImage.BitMap;
    }


    private void button3_Click(object sender, EventArgs e)
    {
      if (CurrentImage == null)  // sanity check: make sure we have an image to manipulate
        return;

      //
      // Example of calling library, which then dumps data about the given image
      // to Visual Studio's "Output" window.  Note you must run with debugging (F5)
      // in order to see the output.
      //
      //PPMImageLibrary.DebugOutput(
      //  CurrentImage.Header.Width,
      //  CurrentImage.Header.Height, 
      //  CurrentImage.Header.Depth,
      //  CurrentImage.ImageListData
      //);

      //
      // we have an image, perform transformation and display new result:
      //
      FSharpList<FSharpList<Tuple<int, int, int>>> newImageList;

      newImageList = PPMImageLibrary.FlipHorizontal(
        CurrentImage.Header.Width,
        CurrentImage.Header.Height,
        CurrentImage.Header.Depth,
        CurrentImage.ImageListData
      );

      //
      // create a new PixelMap here on the client-side, which creates a new bitmap
      // we then display to the user:
      //
      CurrentImage = new PixelMap(newImageList);
      picImage.Image = CurrentImage.BitMap;
    }


    private void button4_Click(object sender, EventArgs e)
    {
      if (CurrentImage == null)  // sanity check: make sure we have an image to manipulate
        return;

      //
      // Example of calling library, which then dumps data about the given image
      // to Visual Studio's "Output" window.  Note you must run with debugging (F5)
      // in order to see the output.
      //
      //PPMImageLibrary.DebugOutput(
      //  CurrentImage.Header.Width,
      //  CurrentImage.Header.Height, 
      //  CurrentImage.Header.Depth,
      //  CurrentImage.ImageListData
      //);

      //
      // we have an image, perform transformation and display new result:
      //
      FSharpList<FSharpList<Tuple<int, int, int>>> newImageList;

      int factor;
      bool success = System.Int32.TryParse(txtFactor.Text, out factor);

      if (!success)
      {
        factor = 2;
        txtFactor.Text = factor.ToString();
      }

      newImageList = PPMImageLibrary.Zoom(
        CurrentImage.Header.Width,
        CurrentImage.Header.Height,
        CurrentImage.Header.Depth,
        CurrentImage.ImageListData,
        factor
      );

      //
      // create a new PixelMap here on the client-side, which creates a new bitmap
      // we then display to the user:
      //
      CurrentImage = new PixelMap(newImageList);
      picImage.Image = CurrentImage.BitMap;
    }


    private void button5_Click(object sender, EventArgs e)
    {
      if (CurrentImage == null)  // sanity check: make sure we have an image to manipulate
        return;

      //
      // Example of calling library, which then dumps data about the given image
      // to Visual Studio's "Output" window.  Note you must run with debugging (F5)
      // in order to see the output.
      //
      //PPMImageLibrary.DebugOutput(
      //  CurrentImage.Header.Width,
      //  CurrentImage.Header.Height, 
      //  CurrentImage.Header.Depth,
      //  CurrentImage.ImageListData
      //);

      //
      // we have an image, perform transformation and display new result:
      //
      FSharpList<FSharpList<Tuple<int, int, int>>> newImageList;

      newImageList = PPMImageLibrary.RotateRight90(
        CurrentImage.Header.Width,
        CurrentImage.Header.Height,
        CurrentImage.Header.Depth,
        CurrentImage.ImageListData
      );

      //
      // create a new PixelMap here on the client-side, which creates a new bitmap
      // we then display to the user:
      //
      CurrentImage = new PixelMap(newImageList);
      picImage.Image = CurrentImage.BitMap;
    }

    private void radioFitImage_CheckedChanged(object sender, EventArgs e)
    {
      if (this.radioFitImage.Checked)
      {
        this.picImage.SizeMode = PictureBoxSizeMode.Zoom;
      }
    }

    private void radioActualSizeImage_CheckedChanged(object sender, EventArgs e)
    {
      if (this.radioActualSizeImage.Checked)
      {
        this.picImage.SizeMode = PictureBoxSizeMode.CenterImage;
      }
    }

  }//class
}//namespace
