


import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;



import com.xuggle.mediatool.IMediaReader;
import com.xuggle.mediatool.MediaListenerAdapter;
import com.xuggle.mediatool.ToolFactory;
import com.xuggle.mediatool.event.IVideoPictureEvent;
import com.xuggle.xuggler.IContainer;

public class Videoread
{
	
    static int kk=1;
    static int k=1;
    static int count=0;
    static int flag=0;
    int temp=0;
    static int start_frame=0;
   static  int countcopy=0;
    static int end_frame=0;
    static int no_of_frames=0;
    private static int mVideoStreamIndex = -1;
    
   
    public static int framecount(String inputFilename)
    {	
      	 IContainer container = IContainer.make();
        int result = container.open(inputFilename, IContainer.Type.READ, null);    
        // check if the operation was successful
        if (result<0)
            throw new RuntimeException("Failed to open media file");
        count = (int) container.getStream(0).getNumFrames();
        container.close();
        countcopy=count;
        System.out.println("Number of Frames " + countcopy);
        if(flag==1)
        {
        if(count>100)
        {
        	count=100;
        }
        no_of_frames=count;
        }
        return countcopy;
    }
    
   public int[][][] defaultread(String inputFilename)
   {	
	   flag=1;
	   kk=1;
	   framecount(inputFilename);
	   IMediaReader mediaReader = ToolFactory.makeReader(inputFilename);
       mediaReader.setBufferedImageTypeToGenerate(BufferedImage.TYPE_3BYTE_BGR);
       ImageSnapListener isl=new ImageSnapListener();
      mediaReader.addListener(isl);
       while (mediaReader.readPacket() == null) ;
       return isl.a;
   }
   
   public int[][][] framesetread(String inputFilename, int start, int end)
   {	
	   flag=2;
	   temp=0;
	   k=1;
	   framecount(inputFilename);
	   start_frame=start;
	   end_frame=end;
	   if((start_frame>=0 && start_frame < end_frame && start_frame < countcopy) && (end_frame>0 && end_frame<=countcopy && end_frame>start_frame) )
	   {
		no_of_frames=end_frame-start_frame+1;
		if(no_of_frames>100)
		{
			no_of_frames=100;
			System.out.println("Total number of frames cannot be greater than 100");
			System.out.println("Printing hundred frames only");
		}
	   IMediaReader mediaReader = ToolFactory.makeReader(inputFilename);
       mediaReader.setBufferedImageTypeToGenerate(BufferedImage.TYPE_3BYTE_BGR);
       ImageSnapListener isl=new ImageSnapListener();
      mediaReader.addListener(isl);
       while (mediaReader.readPacket() == null) ;
    
       return isl.a;
	   }
	   
	   else
	   {
		   System.out.println("Error : Start frame or end frame value not in range of 0 to framecount");
		  
		   int b[][][]=new int [1][100][100];
		  for(int y=1;y<=100;y++)
		  {
			  for(int z=1;z<=100;z++)
			  {
				  b[1][y][z]=0;
			  }
		  }
		return b;
	   }
	   
   }
   
   
    public static void main(String[] args) throws Exception
    {
    	
    	
   
    }
    private class ImageSnapListener extends MediaListenerAdapter {
    	private int a[][][]=new int [Videoread.this.no_of_frames][100][100];

  
        public void onVideoPicture(IVideoPictureEvent event) 
        {
        	
        if (event.getStreamIndex() != mVideoStreamIndex)
        {

               
                if (mVideoStreamIndex == -1)

                    mVideoStreamIndex = event.getStreamIndex();

                else
                    return;
            }
        if(flag==2)
        {
            
                	dumpImageToFileframe(event.getImage());
        }
        if(flag==1)
        {
        	dumpImageToFile(event.getImage());
        }
                	
                	
                	
}
        private void dumpImageToFile(BufferedImage inimage)
        {
        	
        	 BufferedImage image = new BufferedImage(100,100, inimage.getType());
        	 Graphics2D g2d = image.createGraphics();
        	 g2d.drawImage(inimage, 0, 0, 100, 100, null);
        	 g2d.dispose();
        	if(kk>0 && kk<count)
        	{
        		for(int i=0; i<100; i++)
        		{
        	         
                    for(int j=0; j<100; j++)
                    {
                    int x;
                       Color c = new Color(image.getRGB(j, i));
                       int red = (int)(c.getRed() * 0.299);
                       int green = (int)(c.getGreen() * 0.587);
                       int blue = (int)(c.getBlue() *0.114);
                       x=red+green+blue;
                       a[kk-1][i][j]=x;
                       Color newColor = new Color(x,x,x);
                       
                       image.setRGB(j,i,newColor.getRGB());
                    }
                   
                 }
        	kk++;
        	}
        	 
        }	
       
        private void dumpImageToFileframe(BufferedImage inimage)
        {
        	temp++;
        	 BufferedImage image = new BufferedImage(100,100, inimage.getType());
        	 Graphics2D g2d = image.createGraphics();
        	 g2d.drawImage(inimage, 0, 0, 100, 100, null);
        	 g2d.dispose();
        	if((start_frame<temp && temp<=end_frame) && (k>0 && k<=no_of_frames))
        	{
        		for(int i=0; i<100; i++)
        		{
        	         
                    for(int j=0; j<100; j++)
                    {
                    int x;
                       Color c = new Color(image.getRGB(j, i));
                       int red = (int)(c.getRed() * 0.299);
                       int green = (int)(c.getGreen() * 0.587);
                       int blue = (int)(c.getBlue() *0.114);
                       x=red+green+blue;
                       a[temp-start_frame-1][i][j]=x;
                       Color newColor = new Color(x,x,x);              
                       image.setRGB(j,i,newColor.getRGB());
                    }
                   
                 }
        		
        	k++;
        	}
        	 
        }	
       
    }
}


