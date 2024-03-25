using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class Views : BaseMonoClass<Views>
{
    public GameObject[] camera;

    public void CloseAllCameras()
    {
        foreach (var item in camera)
        {
            if (item != null)
            {
                item.SetActive(false);
            }
        }
    }

}
