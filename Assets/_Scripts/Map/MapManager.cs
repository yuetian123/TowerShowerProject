using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class MapManager : BaseMonoClass<MapManager>
{
    public int[] state;
    
    protected override void Awake()
    {
        base.Awake();
        state = new int[5];
    }
    public void Open(int index)
    {
        state[index] = 1;
    }

}
