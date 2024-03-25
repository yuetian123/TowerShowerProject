using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class ChipManager : BaseMonoClass<ChipManager>
{
    protected override void Awake()
    {
        base.Awake();
    }

    public int UnlockCount;

    private void Update()
    {
        if (UnlockCount >= 5)
        {
            //执行收集成功的代码成功
        }
    }
}
